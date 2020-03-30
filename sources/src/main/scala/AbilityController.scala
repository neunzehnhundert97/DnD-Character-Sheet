import Main.info
import Utility.statToModifier
import io.udash.wrappers.jquery.{EventName, JQueryEvent, jQ}
import org.scalajs.dom.Element
import scalatags.JsDom.all._

import JQBootstrapped.jq2modalized

object AbilityController
{
    // Variables for sorting directions
    var abilitySortbyName: Boolean = true

    def readyAbilities(): Unit =
    {
        jQ("#ability-search").on(EventName.keyUp, abilitySearch).value("")
    }

    /** Updates the list of abilities. */
    def updateAbilities(): Unit =
    {
        val data = Mappings.abilities.toList.map(t =>
        {
            val key = t._1
            val value = t._2

            // Add proficiency
            if (info.isProficient(key))
                if (info.isExpert(key))
                    (key, statToModifier(info.score(value)) + info.proficiencyBonus * 2, true, true)
                else
                    (key, statToModifier(info.score(value)) + info.proficiencyBonus, true, false)
            else
                (key, statToModifier(info.score(value)), false, false)
        })

        // Generate html
        jQ("#ability-container").html(
            table(id := "ability-table", cls := "table")(
                tr(
                    th(s"${if (abilitySortbyName) "⮚" else ""}Ability"),
                    th(s"${if (!abilitySortbyName) "⮚" else ""}Mod")
                ),
                for ((key, mod, prof, exp) <- if (abilitySortbyName) data.sortBy(_._1) else data.sortBy(_._2).reverse)
                    yield tr(
                        td(cls := (if (prof) if (exp) "expert-with" else "proficient-with" else ""))(key),
                        td(mod)
                    )
            ).render
        )
            // Add event handlers
            .find("tr:not(:first-child)").on(EventName.click, abilityHandler)

        // Add event handler for sorting
        jQ("#ability-table th:eq(0)").on(EventName.click, (_, _) =>
        {
            abilitySortbyName = true
            updateAbilities()
        })
        jQ("#ability-table th:eq(1)").on(EventName.click, (_, _) =>
        {
            abilitySortbyName = false
            updateAbilities()
        })
    }

    /** Shows and hides abilities depending on the entered search term. */
    private def abilitySearch(elem: Element, event: JQueryEvent): Unit =
    {
        val term = jQ(elem).value().asInstanceOf[String].toLowerCase

        jQ("#ability-table tr").show()
        if (!term.isEmpty)
        {
            jQ("#ability-table tr:not(:first-child)").each((elem, _) =>
            {
                val that = jQ(elem)
                if (!that.children().at(0).text().toLowerCase.contains(term))
                    that.hide()
            })
        }
    }

    /** Shows a modal when clicking on any ability. */
    private def abilityHandler(elem: Element, event: JQueryEvent): Unit =
    {
        val target = jQ(elem)
        val modal = jQ("#ability-modal")
        val profSwitch = modal.find("#switch-is-proficient")
        val expSwitch = modal.find("#switch-is-expertise")
        val ability = target.children().at(0).text()

        // Set up switches
        if (info.isProficient(ability))
        {
            profSwitch.prop("checked", true)
            expSwitch.parent().show()
            expSwitch.prop("checked", info.isExpert(ability))
        } else
        {
            profSwitch.prop("checked", false)
            expSwitch.parent().hide()
        }

        // Set modal title to the selected ability
        modal.find(".modal-title").text(ability)

        // Change handler for proficiency switch
        profSwitch.off()
            .on(EventName.change, (elem, _) =>
            {
                val checked = jQ(elem).prop("checked").asInstanceOf[Boolean]

                if (checked)
                {
                    // Show expertise switch
                    expSwitch.prop("checked", false).parent().show()
                } else
                {
                    // Apply implied effects to expertise
                    expSwitch.prop("checked", false).parent().hide()
                    info.removeExpertise(ability)
                }

                // Set value in information object
                if (checked)
                    info.addProficiency(ability)
                else
                    info.removeProficiency(ability)

                // Update table
                updateAbilities()
            })

        // Change handler for expertise switch
        expSwitch.off()
            .on(EventName.change, (elem, _) =>
            {
                // Set value in information object
                if (jQ(elem).prop("checked").asInstanceOf[Boolean])
                    info.addExpertise(ability)
                else
                    info.removeExpertise(ability)

                // Update table
                updateAbilities()
            })

        // Open modal
        modal.modal("show")
    }
}
