import io.udash.wrappers.jquery._
import org.scalajs.dom.{Element, document, window}
import scalatags.JsDom.all._
import JQBootstrapped.jq2modalized
import org.scalajs.dom.raw.{HTMLFormElement, HTMLTextAreaElement, WheelEvent}
import GlobalScope.encodeURIComponent

import Ordering.Double.TotalOrdering
import scala.scalajs.js

object Main
{
    val dice: List[String] = List("d4", "d6", "d8", "d10", "d12", "d20")

    var info: MainInformation = _

    var abilitySortbyName: Boolean = true
    var inventorySortByColumn: Int = 0

    /** Entry point */
    def main(args: Array[String]): Unit =
    {
        // Register ready handler
        // This is done in this manner as onReady without parenthesis would be called instead of being passed
        jQ(() => onReady())
    }

    /** Ready function to be executed when the document is loaded. Loads config, binds handlers and displays character information. */
    def onReady(): Unit =
    {
        initializeInformation()
        updateTitle()
        updateAttributes()
        updateAbilities()
        updateWeaponList()
        updateInventory()

        // Show a warning when trying to reload the page
        window.addEventListener("beforeunload", (e: js.Dynamic) =>
        {
            e.preventDefault()
        })

        // Define click handler for attributes
        jQ("#attribute-table .row-stat").on(EventName.click, attributeHandler)

        // Define click handler for buttons
        jQ("#button-import").on(EventName.click, openImportModal)
        jQ("#import-string-button").on(EventName.click, importHandler)
        jQ("#button-export").on(EventName.click, openExportModal)
        jQ("#button-save").on(EventName.click, saveHandler)

        // Set click handler for general information
        jQ("#general-stats").on(EventName.click, openGeneralModal)

        // Change handler for elements in general modal
        jQ("#general-modal input[name=name]").on(EventName.change, nameHandler)
        jQ("#general-modal select[name=level]").on(EventName.change, levelHandler)
        jQ("#general-modal input[name=experience]").on(EventName.change, experienceHandler)
        jQ("#general-modal input[name=class]").on(EventName.change, classHandler)
        jQ("#general-modal input[name=race]").on(EventName.change, raceHandler)

        // Handler for weapons
        jQ("#add-weapon-button").on(EventName.click, openNewWeaponModal)
        jQ("#create-new-weapon").on(EventName.click, createNewWeaponHandler)
        jQ("#delete-weapon").on(EventName.click, removeWeapon)
        jQ("#weapon-modal [name=weapon-hand]").on(EventName.change, weaponHandHandler)
        jQ("#weapon-modal [name=weapon-type]").on(EventName.change, weaponTypeHandler)
        jQ("#weapon-is-thrown").on(EventName.change, weaponThrownHandler)
        jQ("#weapon-modal [name=weapon-die-type]").on(EventName.change, (_, _) => jQ("#weapon-modal [name=weapon-hand]").trigger(EventName.change))

        jQ("#add-item-button").on(EventName.click, openNewItemModal)
        jQ("#create-new-item").on(EventName.click, createupdateItem)
        jQ("#delete-item").on(EventName.click, removeItem)

        // Register search handlers
        jQ("#ability-search").on(EventName.keyUp, abilitySearch).value("")
        jQ("#weapon-search").on(EventName.keyUp, weaponSearch).value("")
        jQ("#inventory-search").on(EventName.keyUp, inventorySearch).value("")
    }

    /** Loads information embedded in the documents or the default. */
    private def initializeInformation(): Unit =
    {
        // Load embedded JSON string from HTML
        val embeddedInfo = jQ("#information-container").text().replace("\n", "")
        var loadSuccessful = false

        // Try to process it if existing
        try
        {
            if (!embeddedInfo.isEmpty)
            {
                info = MainInformation.fromJSON(embeddedInfo)
                loadSuccessful = true
                println("Loaded embedded information")
            }
        }
        catch
        {
            case e: Throwable =>
                println("There was an error while loading the embedded information: " + e)
        }

        // If there was no information string or it was invalid, the default information object is created
        if (!loadSuccessful)
        {
            println("Loading default information")
            info = new MainInformation()
        }
    }

    /** Updates the document's title and navbar. */
    def updateTitle(): Unit =
    {
        // Load header information
        document.title = "D&D Sheet - " + info.name
        jQ("#general-stats").text(s"${info.name}, ${info.level} Level ${info.cls} (${info.race})")
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

    /** Updates the list of weapons. */
    def updateWeaponList(): Unit =
    {
        val contents = info.weapons.zipWithIndex.iterator.map(t =>
        {
            val (weapon, index) = t

            // Compute modifier
            val mod = if (weapon.melee && (!weapon.finesse || info.score("str") > info.score("dex")))
                statToModifier(info.score("str"))
            else
                statToModifier(info.score("dex"))

            val damage = weapon.damageBonus + mod
            val toHit = mod + (if (weapon.proficiency)
            // Apply proficiency
                weapon.hitBonus + info.proficiencyBonus
            else weapon.hitBonus)


            val count = if (weapon.dieCount > 1) weapon.dieCount.toString else ""

            // Prepare dis play damage
            val damageString = s"$count${weapon.die}${
                if (weapon.versatile) "/" + dice(dice.indexOf(weapon.die) + 1) else ""
            } ${
                if (damage != 0)
                    f"$damage%+d"
                else
                    ""
            }"

            val range = if (!weapon.melee || weapon.thrown)
                s"${weapon.shortRange} / ${weapon.longRange}"
            else "-"

            tr(cls := "not-selectable", data.index := index)(td(weapon.name), td(range), td(toHit), td(damageString))
        }).toList

        jQ("#weapon-container").html(
            table(id := "weapon-table", cls := "table")(
                tr(th("Weapon"), th("range"), th("to hit"), th("damage")),
                contents
            ).render
        ).find("tr:not(:first-child)").on(EventName.click, weaponHandler)

        // Show the search bar when there are at least 4 items
        if (info.weapons.length > 3)
            jQ("#weapon-search").show()
        else
            jQ("#weapon-search").hide()
    }

    def updateInventory(): Unit =
    {
        val inventory = (inventorySortByColumn match
        {
            case 1 => info.inventory.sortBy(i => i.price * Mappings.currencies(i.priceUnit)).reverse
            case 2 => info.inventory.sortBy(_.weight).reverse
            case _ => info.inventory.sortBy(_.name)
        }).toList

        // Generate table
        jQ("#inventory-container").html(
            table(id := "inventory-table", cls := "table")(
                tr(
                    th(s"${if (inventorySortByColumn == 0) "⮚" else ""}Item"),
                    th(s"${if (inventorySortByColumn == 1) "⮚" else ""}Price"),
                    th(s"${if (inventorySortByColumn == 2) "⮚" else ""}Weight")
                ),
                for ((item, index) <- inventory.zipWithIndex)
                    yield tr(data.index := index, data.hash := item.##)(
                        td(s"${item.amount} ${item.name}"),
                        td(if (item.price != 0) s"${item.amount.toIntOption.getOrElse(1) * item.price} ${item.priceUnit}" else "-"),
                        td(if (item.weight != 0) item.amount.toIntOption.getOrElse(1) * item.weight else "-")
                    ),
            ).render
        ).find("tr:not(:first-child)").on(EventName.click, modifyItem)

        // Add event handler for sorting
        jQ("#inventory-table th:eq(0)").on(EventName.click, (_, _) =>
        {
            inventorySortByColumn = 0
            updateInventory()
        })
        jQ("#inventory-table th:eq(1)").on(EventName.click, (_, _) =>
        {
            inventorySortByColumn = 1
            updateInventory()
        })
        jQ("#inventory-table th:eq(2)").on(EventName.click, (_, _) =>
        {
            inventorySortByColumn = 2
            updateInventory()
        })

        // Show the search bar when there are at least 4 items
        if (info.inventory.length > 3)
            jQ("#inventory-search").show()
        else
            jQ("#inventory-search").hide()
    }

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

        updateAttributes()
        updateAbilities()

        jQ("#attribute-modal").modal("hide")
    }

    /** Handles a changed proficiency for a ability. */
    private def attributeProficiencyHandler(attribute: String)(elem: Element, event: JQueryEvent): Unit =
        if (jQ(elem).prop("checked").asInstanceOf[Boolean])
        // Update proficiency
            info.addProficiency(attribute.toLowerCase)
        else
            info.removeProficiency(attribute.toLowerCase)


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

    /** Shows and hides items in the inventory depending on the entered search term. */
    private def inventorySearch(elem: Element, event: JQueryEvent): Unit =
    {
        val term = jQ(elem).value().asInstanceOf[String].toLowerCase

        jQ("#inventory-table tr").show()
        if (!term.isEmpty)
        {
            jQ("#inventory-table tr:not(:first-child)").each((elem, _) =>
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

    /** Handles a changed class. */
    private def classHandler(elem: Element, event: JQueryEvent): Unit =
    {
        info.cls = jQ(elem).value().asInstanceOf[String]
        updateTitle()
    }

    /** Handles changed experience. */
    private def experienceHandler(elem: Element, event: JQueryEvent): Unit =
    {
        val xp = jQ(elem).value().asInstanceOf[String].toIntOption.getOrElse(0)

        // Find matching level
        val level: Int = Mappings.levels.toList.sorted.reverse.find(map => map._2._1 < xp).map(_._1).getOrElse(1)

        // Set xp and level
        info.experience = xp
        info.level = level

        // Update shown info
        jQ("#general-modal select[name=level]").value(level)
        updateTitle()
    }

    /** Handles a changed level. */
    private def levelHandler(elem: Element, event: JQueryEvent): Unit =
    {
        val level = jQ(elem).value().asInstanceOf[String].toInt

        // Update info object
        info.level = level
        info.experience = Mappings.levels(level)._1

        // Update shown info
        jQ("#general-modal input[name=experience]").value(Mappings.levels(level)._1)
        updateTitle()
    }

    /** Handles a changed name. */
    private def nameHandler(elem: Element, event: JQueryEvent): Unit =
    {
        info.name = jQ(elem).value().asInstanceOf[String]
        updateTitle()
    }

    /** Handles a changed race. */
    private def raceHandler(elem: Element, event: JQueryEvent): Unit =
    {
        info.race = jQ(elem).value().asInstanceOf[String]
        updateTitle()
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

    /** */
    private def createupdateItem(elem: Element, event: JQueryEvent): Unit =
    {
        val formData = new FormData(document.getElementById("item-form").asInstanceOf[HTMLFormElement])
        val name = formData.get("name")

        if (name.isEmpty)
        {
            showAlert("#item-modal .alert-container", "Name is missing")
            return
        }

        val item = new Item(name, formData.get("amount"), formData.get("price").toIntOption.getOrElse(0), formData.get("priceUnit"), formData.get("weight").toDoubleOption.getOrElse(0), formData.get("notes"))

        formData.get("hash").toIntOption match
        {
            case Some(hash) => info.replaceItemByHash(hash, item)
            case None => info.addItem(item)
        }

        jQ("#item-modal").modal("hide")
        updateInventory()
    }

    /** */
    private def modifyItem(elem: Element, event: JQueryEvent): Unit =
    {
        val hash = jQ(elem).attr("data-hash").flatMap(_.toIntOption).get
        val item = info.item(hash)

        jQ("#item-modal [name=name]").value(item.name)
        jQ("#item-modal [name=price]").value(item.price)
        jQ("#item-modal [name=priceUnit]").value(item.priceUnit)
        jQ("#item-modal [name=weight]").value(item.weight)
        jQ("#item-modal [name=amount]").value(item.amount)
        jQ("#item-modal [name=notes]").value(item.notes)
        jQ("#item-modal [name=hash]").value(hash)
        jQ("#item-modal .alert").remove()
        jQ("#item-modal #create-new-item").text("Modify item")
        jQ("#delete-item").show()
        jQ("#item-modal").modal("show")
    }

    /** Removes the item currently opened in the modal. */
    private def removeItem(elem: Element, event: JQueryEvent): Unit =
    {
        val hash = jQ("#item-modal [name=hash]").value().asInstanceOf[String].toInt
        info.removeItem(hash)
        jQ("#item-modal").modal("hide")
        updateInventory()
    }

    private def healthPointsHandler(event: WheelEvent): Unit =
    {
        event.preventDefault()

        val elem = jQ(event.currentTarget)
        val hp = elem.text().split(": ")(1).split(" / ")(0).toInt

        // Decide whether the wheel went up or down
        if (event.deltaY < 0 && hp < info.maxHP)
            elem.text(s"HP: ${hp + 1} / ${info.maxHP}")
        else if (event.deltaY > 0 && hp > 0)
            elem.text(s"HP: ${hp - 1} / ${info.maxHP}")
    }

    /** Open the import modal. */
    private def openImportModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#import-string").value("")
        jQ("#import-modal").modal("show")
    }

    /** Tries to import and parse the given JSON string. */
    private def importHandler(elem: Element, event: JQueryEvent): Unit =
        try
        {
            val input = jQ("#import-string").value().asInstanceOf[String]
            info = MainInformation.fromJSON(input)
            jQ("#import-modal").modal("hide")

            // Apply read information
            updateTitle()
            updateAttributes()
            updateAbilities()
            updateWeaponList()
            updateInventory()
        } catch
        {
            case _: Throwable =>
                window.alert("Malformed input!")
        }


    /** Shows the export modal, fills in the string and selects it. */
    private def openExportModal(elem: Element, event: JQueryEvent): Unit =
    {
        // Set export string
        jQ("#export-string").text(info.toJSON)
        // Display modal
        jQ("#export-modal").modal("show")
        // Select all text after short delay
        window.setTimeout(() => jQ("#export-string").get(0) match
        {
            case Some(i: HTMLTextAreaElement) =>
                i.focus()
                i.select()
            case _ =>
        }, 200)
    }

    /** Opens the modal for general information. */
    private def openGeneralModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#general-modal input[name=name]").value(info.name)
        jQ("#general-modal select[name=level]").value(info.level)
        jQ("#general-modal input[name=experience]").value(info.experience)
        jQ("#general-modal input[name=class]").value(info.cls)
        jQ("#general-modal input[name=race]").value(info.race)
        jQ("#item-modal #create-new-item").text("Create item")
        jQ("#general-modal").modal("show")
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

    private def openNewItemModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#item-modal .modal-title").text("New item")
        jQ("#item-modal input").value("")
        jQ("#item-modal textarea").value("")
        jQ("#delete-item").hide()
        jQ("#item-modal .alert").remove()
        jQ("#item-modal").modal("show")
    }

    /** Shows a save dialog. */
    private def saveHandler(elem: Element, event: JQueryEvent): Unit =
    {
        // Write information into file
        jQ("#information-container").text(info.toJSON)

        // Generate file content out of the document
        val content = "data:plain/attachment," + encodeURIComponent(document.documentElement.innerHTML)

        // Create a element with download
        val element = a(href := content, attr("download") := s"${info.name} - Sheet.html", style := "display: none").render

        // Insert it, click it, and remove it
        document.body.appendChild(element)
        element.click()
        document.body.removeChild(element)
    }

    private def showAlert(selector: Selector, msg: String): Unit =
        jQ(selector).html(div(cls := "alert alert-danger alert-dismissible fade show")(
            button(`type` := "button", cls := "close", data.dismiss := "alert")(raw("&times")),
            span(msg)
        ).render)

    /** Convert an attribute into a modifier. */
    def statToModifier(stat: Int): Int =
        Math.floor((stat - 10) / 2).toInt
}