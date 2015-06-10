package org.ladderframework.html

/**
 * @author skytteren
 */
import org.ladderframework.Context
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import org.scalatest.FunSpec
import org.ladderframework.form._
import org.ladderframework.form.named._
import org.ladderframework.html.form._
import org.ladderframework.js.JsNoCmd
import org.ladderframework.StatefulHtmlPage
import scala.concurrent.ExecutionContext.Implicits.global

class FormBindingSpec extends FunSpec{
  
  describe("Form bind simple"){
    val per = Speaker("Per", Date(1, 12, 1924), None, Nil)
    val page = new ProductHtmlPage(Some(per))
    it("it compiles"){
      // YEAH
    }
  }
}

case class Speaker(name: String, date: Date, info: Option[String], addresses: List[(Boolean, Address)])
case class Date(day: Int, month: Int, year: Int)
case class Address(street: String, city: String, code: Int)


class ProductHtmlPage(speaker: Option[Speaker])(implicit ec:ExecutionContext) extends StatefulHtmlPage {
  val source: String = "speaker.html"
    
  val form = Form("speaker" --> mapping(Speaker)(Speaker.unapply _)(
    "name" --> of[String], 
    "date" --> mapping(Date.apply _)( Date.unapply _)("day" --> of[Int], "month" --> of[Int], "year" --> of[Int]),
    "info" --> optional(of[String]),
    "addresses" --> list(
        mapping((selected: Boolean, address: Address) => (selected -> address))
            ((touple: Tuple2[Boolean, Address]) => Option(touple._1 -> touple._2))(
            "selected" --> boolean, 
            "address" --> mapping(Address)(Address.unapply _)("street" --> of[String], "city" --> of[String], "code" --> of[Int])
        )
    )
  ))
  
  def render(implicit context: Context): Future[NodeSeq => NodeSeq] = Future{
    Ajax(form)((either, id) => Future(JsNoCmd))(
        implicit context => speaker => {
          ns => {
            <div>
              <h3>Speaker</h3>
              {
                speaker.render((name, date, info, addresses) => {
                  date.render((day, month, year) => ns => {
                    name.text("Name", 'id -> "name") ++
                    <fieldset>
                      <legend>Date of birth:</legend>
                      {
                        day.text("Day") ++
                        month.text("Month") ++
                        year.text("Year")
                      }
                    </fieldset> /*++
                    info.textarea("Info")*/
                  })
                })
              }
              <input type="submit" value="Send"/>
            </div>
          }
        }
    )
    ns => ns
  }
}