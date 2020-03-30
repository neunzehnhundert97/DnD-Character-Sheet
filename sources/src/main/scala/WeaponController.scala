import Main.{dice, info}
import Utility._
import io.udash.wrappers.jquery.{EventName, JQueryEvent, jQ}
import org.scalajs.dom.Element
import scalatags.JsDom.all._
import JQBootstrapped.jq2modalized

object WeaponController
{
    def readyWeapons(): Unit =
    {
        // Handler for weapons
        jQ("#add-weapon-button").on(EventName.click, openNewWeaponModal)
        jQ("#create-new-weapon").on(EventName.click, createNewWeaponHandler)
        jQ("#delete-weapon").on(EventName.click, removeWeapon)
        jQ("#weapon-modal [name=weapon-hand]").on(EventName.change, weaponHandHandler)
        jQ("#weapon-modal [name=weapon-type]").on(EventName.change, weaponTypeHandler)
        jQ("#weapon-is-thrown").on(EventName.change, weaponThrownHandler)
        jQ("#weapon-modal [name=weapon-die-type]").on(EventName.change, (_, _) => jQ("#weapon-modal [name=weapon-hand]").trigger(EventName.change))

        jQ("#weapon-search").on(EventName.keyUp, weaponSearch).value("")
    }

    /** Updates the list of weapons. */
    def updateWeaponList(): Unit =
    {
        jQ("#weapon-container").html(
            table(id := "weapon-table", cls := "table")(
                tr(th("Weapon"), th("range"), th("to hit"), th("damage")),
                for ((weapon, index) <- info.weapons.zipWithIndex)
                    yield tr(cls := "not-selectable", data.index := index)(
                        td(weapon.name),
                        td(weapon.rangeString),
                        td(weapon.toHit),
                        td(weapon.damageString)
                    )
            ).render
        ).find("tr:not(:first-child)").on(EventName.click, weaponHandler)

        // Show the search bar when there are at least 4 items
        if (info.weapons.length > 3)
            jQ("#weapon-search").show()
        else
            jQ("#weapon-search").hide()
    }


    /** Shows and hides weapons depending on the entered search term. */
    private def weaponSearch(elem: Element, event: JQueryEvent): Unit =
    {
        val term = jQ(elem).value().asInstanceOf[String].toLowerCase

        jQ("#weapon-table tr").show()
        if (!term.isEmpty)
        {
            jQ("#weapon-table tr:not(:first-child)").each((elem, _) =>
            {
                val that = jQ(elem)
                if (!that.children().at(0).text().toLowerCase.contains(term))
                    that.hide()
            })
        }
    }

    /** Handles a changed weapon type and hides or display elements accordingly. */
    private def weaponTypeHandler(elem: Element, event: JQueryEvent): Unit =
        changeWeaponModalMeleeOrRange(jQ(elem).value().asInstanceOf[String] == "melee")

    /** */
    private def changeWeaponModalMeleeOrRange(melee: Boolean): Unit =
        if (melee)
        {
            jQ("#weapon-modal div.weapon-range").hide()
            jQ("#weapon-is-thrown").parent().show()
            jQ("#weapon-is-reached").parent().show()
            jQ("#weapon-is-loading").parent().hide()
            jQ("#weapon-modal select[name=weapon-hand] option:eq(2)").show()
        }
        else
        {
            jQ("#weapon-modal div.weapon-range").show()
            jQ("#weapon-is-thrown").parent().hide()
            jQ("#weapon-is-reached").parent().hide()
            jQ("#weapon-is-loading").parent().show()
            jQ("#weapon-modal select[name=weapon-hand] option:eq(2)").hide()
        }

    /** Handles the thrown property and hides or shows the range inputs. */
    private def weaponThrownHandler(elem: Element, event: JQueryEvent): Unit =
        if (jQ(elem).prop("checked").asInstanceOf[Boolean])
            jQ("#weapon-modal div.weapon-range").show()
        else
            jQ("#weapon-modal div.weapon-range").hide()


    /** Handles the weapon hand property and hides or shows inputs. */
    private def weaponHandHandler(elem: Element, event: JQueryEvent): Unit =
        jQ(elem).value().asInstanceOf[String] match
        {
            case "Versatile" =>
                jQ("#weapon-is-heavy").prop("checked", false).parent().hide()
                jQ("#weapon-is-finesse").prop("checked", false).parent().hide()
                jQ("#weapon-modal .versatile-die").show().text("/ " + dice(dice.indexOf(jQ("#weapon-modal [name=weapon-die-type]").value().asInstanceOf[String]) + 1))
            case "Two-Handed" =>
                jQ("#weapon-is-heavy").parent().show()
                jQ("#weapon-is-finesse").prop("checked", false).parent().hide()
                jQ("#weapon-modal .versatile-die").hide()
            case _ =>
                jQ("#weapon-is-heavy").prop("checked", false).parent().hide()
                jQ("#weapon-is-finesse").parent().show()
                jQ("#weapon-modal .versatile-die").hide()
        }

    /** Creates a new weapon or changes an existing one from the given input. */
    private def createNewWeaponHandler(elem: Element, event: JQueryEvent): Unit =
    {
        val name = jQ("#weapon-modal [name=weapon-name]").value().asInstanceOf[String]
        val melee = jQ("#weapon-modal [name=weapon-type]").value().asInstanceOf[String] == "melee"
        val die = jQ("#weapon-modal [name=weapon-die-type]").value().asInstanceOf[String]
        val hitBonus = jQ("#weapon-modal [name=weapon-hit-bonus]").value().asInstanceOf[String]
        val damageBonus = jQ("#weapon-modal [name=weapon-damage-bonus]").value().asInstanceOf[String]
        val damageType = jQ("#weapon-modal [name=weapon-damage-type]").value().asInstanceOf[String]
        val index = jQ("#weapon-modal [name=index]").value().asInstanceOf[String].toIntOption

        // Check for name
        if (name.length == 0)
        {
            showAlert("#weapon-modal .alert-container", "The weapon needs to have a name")
            return
        }

        // Check for damage
        val count = jQ("#weapon-modal [name=weapon-dice-number]").value().asInstanceOf[String].toIntOption match
        {
            case Some(value) =>
                value
            case None =>
                showAlert("#weapon-modal .alert-container", "Count missing or not a number")
                return
        }

        val weapon = new Weapon(name,
            die,
            count,
            melee,
            damageType,
            jQ("#weapon-modal [name=notes]").value().asInstanceOf[String],
            hitBonus.toIntOption.getOrElse(0),
            damageBonus.toIntOption.getOrElse(0),
            jQ("#weapon-is-proficient").prop("checked").asInstanceOf[Boolean],
            jQ("#weapon-is-finesse").prop("checked").asInstanceOf[Boolean],
            jQ("#weapon-is-heavy").prop("checked").asInstanceOf[Boolean],
            jQ("#weapon-is-reached").prop("checked").asInstanceOf[Boolean],
            jQ
            ("#weapon-is-loading").prop("checked").asInstanceOf[Boolean],
            jQ("#weapon-modal [name=weapon-hand]").value().asInstanceOf[String] == "Light",
            jQ("#weapon-modal [name=weapon-hand]").value().asInstanceOf[String] == "Versatile",
            jQ("#weapon-modal [name=weapon-hand]").value().asInstanceOf[String] == "Two-Handed",
            jQ("#weapon-is-thrown").prop("checked").asInstanceOf[Boolean],
            jQ("#weapon-modal [name=short-range]").value().asInstanceOf[String].toIntOption.getOrElse(0),
            jQ("#weapon-modal [name=long-range]").value().asInstanceOf[String].toIntOption.getOrElse(0)
        )

        // Add or replace the new weapon
        index match
        {
            case Some(index) =>
                info.replaceWeapon(index, weapon)
            case None =>
                info.appWeapon(weapon)
        }

        updateWeaponList()
        jQ("#weapon-modal").modal("hide")
    }

    /** Removes the weapon currently opened in the modal. */
    private def removeWeapon(elem: Element, event: JQueryEvent): Unit =
    {
        val index = jQ("#weapon-modal [name=index]").value().asInstanceOf[String].toInt
        info.removeWeapon(index)
        jQ("#weapon-modal").modal("hide")
        updateWeaponList()
    }

    /** Opens the weapon modal to edit an existing one. */
    private def weaponHandler(elem: Element, event: JQueryEvent): Unit =
    {
        event.preventDefault()

        val weapon = info.weapon(jQ(elem).attr("data-index").get.toInt)

        jQ("#weapon-modal .modal-title").text("Modify weapon")

        // Insert information
        jQ("#weapon-modal [name=weapon-name]").value(weapon.name)
        jQ("#weapon-modal [name=weapon-type]").value(if (weapon.melee) "melee" else "ranged")
        jQ("#weapon-modal [name=weapon-dice-number]").value(weapon.dieCount)
        jQ("#weapon-modal [name=weapon-die-type]").value(weapon.die)
        jQ("#weapon-modal [name=weapon-damage-type]").value(weapon.damageType)
        jQ("#weapon-modal [name=notes]").value(weapon.notes)
        jQ("#weapon-modal #weapon-is-proficient").prop("checked", weapon.proficiency)
        jQ("#weapon-modal #weapon-is-reached").prop("checked", weapon.reach)
        if (weapon.light)
        {
            jQ("#weapon-modal [name=weapon-hand]").value("Light")
            jQ("#weapon-modal #weapon-is-finesse").prop("checked", weapon.finesse)
        } else if (weapon.versatile)
        {
            jQ("#weapon-modal [name=weapon-hand]").value("Versatile")
        } else if (weapon.twoHanded)
        {
            jQ("#weapon-modal [name=weapon-hand]").value("Two-Handed")
            jQ("#weapon-modal #weapon-is-heavy").prop("checked", weapon.heavy)
        } else
            jQ("#weapon-modal [name=weapon-hand]").value("Hand property")

        jQ("#weapon-modal [name=weapon-hit-bonus]").value(weapon.hitBonus)
        jQ("#weapon-modal [name=weapon-damage-bonus]").value(weapon.damageBonus)
        jQ("#weapon-modal [name=weapon-short-range]").value(weapon.shortRange)
        jQ("#weapon-modal [name=weapon-long-range]").value(weapon.longRange)
        jQ("#weapon-modal [name=index]").value(jQ(elem).attr("data-index").get.toInt)
        jQ("#create-new-weapon").text("Modify weapon")
        jQ("#delete-weapon").show()

        // Hide or show fields
        changeWeaponModalMeleeOrRange(weapon.melee)

        if (weapon.thrown)
            jQ("#weapon-modal div.weapon-range").show()

        jQ("#weapon-modal .alert").remove()
        jQ("#weapon-modal").modal("show")
    }

    /** Opens the weapon modal. */
    private def openNewWeaponModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#weapon-modal .modal-title").text("New weapon")
        jQ("#weapon-modal input").value("")
        jQ("#weapon-modal [name=weapon-type]").value("melee")
        jQ("#weapon-modal select[name=weapon-hand]").value("Hand property").trigger(EventName.change)
        jQ("#weapon-modal select[name=weapon-die-type]").value("d6")
        jQ("#weapon-modal [type=checkbox]").prop("checked", false)
        jQ("#weapon-is-loading").parent().hide()
        jQ("#create-new-weapon").text("Add weapon")
        jQ("#weapon-modal [name=notes]").value("")
        jQ("#delete-weapon").hide()
        jQ("#weapon-modal div.weapon-range").hide()
        jQ("#weapon-modal .alert").remove()
        jQ("#weapon-modal").modal("show")
    }

}
