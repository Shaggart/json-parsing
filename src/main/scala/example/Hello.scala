package example

import java.io.PrintWriter
import java.io.File
import scala.io.Source
import io.circe.parser
import io.circe.Encoder
import io.circe.Decoder.Result
import io.circe.Json
import io.circe._
import io.circe.syntax._
object Hello extends App {
  val url = "https://raw.githubusercontent.com/mledoze/countries/master/countries.json"
  val res = Source.fromURL(url).mkString
  val curRegion: String = "Africa"

   //val doc: Json = parse(res).getOrElse(Json.Null)
   case class Countries(region:String, official: String, capital: Option[String] = None, area: Double)
   object Countries{
      implicit val decoder: Decoder[Countries] = new Decoder[Countries] {
        override def apply(hCursor: HCursor): Result[Countries] =

          for {
            region <- hCursor.downField("region").as[String]
            official <- hCursor.downField("name").downField("official").as[String]
            capital <- hCursor.downField("capital").downN(0).as[Option[String]]
            area <- hCursor.downField("area").as[Double]

          } yield {
            Countries(region, official, capital, area)
          }
      }

      implicit val encoder: Encoder[Countries] = new Encoder[Countries] {
        override def apply(a: Countries): Json =
          Json.obj(
            "name" -> a.official.asJson,
            "capital" -> a.capital.asJson,
            "area" -> a.area.asJson
          )
      }
   }

  val parseResult = parser.decode[List[Countries]](res) match {
    case Right(rest) => rest.filter(_.region.contains(curRegion)).sortBy(-_.area).take(10)
  }


  val intsJson = parseResult.asJson

  val outputFile = args(0)

  val printWriter = new PrintWriter(new File(outputFile))
  printWriter.println(intsJson)
  printWriter.flush
  printWriter.close()

}