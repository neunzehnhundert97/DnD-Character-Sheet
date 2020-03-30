import Main.info
import Utility.statToModifier
import io.udash.wrappers.jquery.{EventName, JQuery, JQueryEvent, jQ}
import org.scalajs.dom.Element

import JQBootstrapped.jq2modalized

object AttributeController
{
    /** Bind handlers for everything involving the attributes. */
    def readyAttributes(): Unit =
    {
        // Define click handler for attributes
        jQ("#attribute-table .row-stat").on(EventName.click, attributeHandler)
    }

    /** Updates the attributes and saving throws. */
    def updateAttributes(): Unit =
        jQ("#attribute-table tr.row-stat").each((elem, _) =>
        {
            val attribute = jQ(elem).find("td:first-child").text()
            val stat: Int = info.score(attribute)
            val mod: Int = statToModifier(stat)
            val save: Int = if (info.isProficient(jQ(elem).children().at(0).text()))
                mod + info.proficiencyBonus
            else mod

            // Update table
            jQ(elem).children().at(1).text(stat)
            jQ(elem).children().at(2).text(mod)
            jQ(elem).children().at(3).text(save)
        })

    /** Shows a modal on clicking on any attribute. */
    private def attributeHandler(elem: Element, event: JQueryEvent): Unit =
    {
        // Get clicked row and values
        val target = jQ(elem)
        val attribute: String = target.children().at(0).text()
        val value: Int = target.children().at(1).text().toInt
        val modifier: Int = statToModifier(value)

        // Set modal title
        jQ("#attribute-modal .modal-title").text("Change " + attribute)

        // Set new change handler for the select element, remove old ones, and set it to the current value
        jQ("#attribute-modal select")
            .off()
            .value(s"$value ($modifier" + ")")
            .on(EventName.change, attributeScoreHandler(attribute, target))

        jQ("#attribute-modal input")
            .off()
            .prop("checked", info.isProficient(attribute))
            .on(EventName.change, attributeProficiencyHandler(attribute))

        jQ("#attribute-modal").modal("show")
    }

    /** Handles a change attribute score. */
    private def attributeScoreHandler(attribute: String, target: JQuery)(elem: Element, event: JQueryEvent): Unit =
    {
        // Get selected values
        val values: Array[String] = jQ(elem).value().asInstanceOf[String].split("\\(")
        val score = values(0).trim.toInt

        // Write values into table and info object
        info.score(attribute) = score

        jQ("#attribute-modal").modal("hide")
    }

    /** Handles a changed proficiency for a ability. */
    private def attributeProficiencyHandler(attribute: String)(elem: Element, event: JQueryEvent): Unit =
        if (jQ(elem).prop("checked").asInstanceOf[Boolean])
        // Update proficiency
            info.addProficiency(attribute.toLowerCase)
        else
            info.removeProficiency(attribute.toLowerCase)
}
