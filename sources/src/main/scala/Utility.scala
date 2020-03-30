import io.udash.wrappers.jquery.{Selector, jQ}
import scalatags.JsDom.all._

object Utility
{

    /** Creates an alert in the given element with the given text. */
    def showAlert(selector: Selector, msg: String): Unit =
        jQ(selector).html(div(cls := "alert alert-danger alert-dismissible fade show")(
            button(`type` := "button", cls := "close", data.dismiss := "alert")(raw("&times")),
            span(msg)
        ).render)

    /** Convert an attribute into a modifier. */
    def statToModifier(stat: Int): Int =
        Math.floor((stat - 10.0) / 2).toInt
}
