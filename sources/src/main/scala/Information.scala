import Main.{dice, info, updateAll, updateTitle}
import Utility._
import WeaponController._
import InventoryController._
import StatusController._
import AbilityController._
import AttributeController._

import scala.collection.mutable.{ListBuffer, Map => MutableMap, Set => MutableSet}
import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.{Dictionary, JSON, |}

class MainInformation(_general: General = new General(),
                      attributes: MutableMap[String, Int] = MutableMap(
                          "str" -> 10,
                          "dex" -> 10,
                          "con" -> 10,
                          "int" -> 10,
                          "wis" -> 10,
                          "cha" -> 10
                      ),
                      _status: MutableMap[String, Int] = MutableMap(
                          "maxHP" -> 6,
                          "currentHP" -> 6,
                          "tempHP" -> 0,
                          "ac" -> 10,
                          "usedHitDie" -> 0
                      ),
                      _customStatus: MutableMap[String, CustomStat] = MutableMap(),
                      proficiency: MutableSet[String] = MutableSet(),
                      expertise: MutableSet[String] = MutableSet(),
                      _weapons: ListBuffer[Weapon] = ListBuffer(),
                      val inventory: ListBuffer[Item] = ListBuffer(),
                      val notes: ListBuffer[Note] = ListBuffer())
{

    /** Returns if there is a proficiency with this ability. */
    def isProficient(ability: String): Boolean =
        proficiency.contains(ability.toLowerCase)

    def addProficiency(ability: String): Unit =
    {
        proficiency += ability.toLowerCase
        updateAttributes()
        updateAbilities()
    }

    def removeProficiency(ability: String): Unit =
    {
        proficiency -= ability.toLowerCase
        updateAttributes()
        updateAbilities()
    }

    def isExpert(ability: String): Boolean =
        expertise.contains(ability.toLowerCase)

    def addExpertise(ability: String): Unit =
        expertise += ability.toLowerCase

    def removeExpertise(ability: String): Unit =
        expertise -= ability.toLowerCase

    def proficiencyBonus: Int =
        Mappings.levels(_general.level)._2

    def name: String =
        _general.name

    def name_=(s: String): Unit =
    {
        _general.name = s
        updateTitle()
    }

    def race: String =
        _general.race

    def race_=(s: String): Unit =
    {
        _general.race = s
        updateTitle()
    }

    def cls: String =
        _general.`class`

    def cls_=(s: String): Unit =
    {
        _general.`class` = s
        updateTitle()
    }

    def level: Int =
        _general.level

    def level_=(i: Int): Unit =
    {
        _general.level = i
        updateAll()
    }

    def experience: Int =
        _general.experience

    def experience_=(i: Int): Unit =
        _general.experience = i

    def status: MutableMap[String, Int] =
        _status

    def maxHP: Int =
        _status("maxHP")

    def maxHP_=(hp: Int): Unit =
    {
        _status("maxHP") = hp
        updateHealth()
    }

    def currentHP: Int =
        _status("currentHP")

    def currentHP_=(hp: Int): Unit =
    {
        _status("currentHP") = hp
        updateHealth()
    }

    def tempHP: Int =
        _status("tempHP")

    def tempHP_=(hp: Int): Unit =
    {
        _status("tempHP") = hp
        updateHealth()
    }

    def maxHitDie: Int =
        level

    def usedHitDie: Int =
        _status("usedHitDie")

    def usedHitDie_=(die: Int): Unit =
        _status("usedHitDie") = die

    def customStatus: MutableMap[String, CustomStat] =
        _customStatus

    def weapon(index: Int): Weapon =
        _weapons(index)

    def weapons: List[Weapon] =
        _weapons.toList

    def appWeapon(weapon: Weapon): Unit =
    {
        _weapons += weapon
        updateWeaponList()
    }

    def replaceWeapon(index: Int, weapon: Weapon): Unit =
    {
        _weapons(index) = weapon
        updateWeaponList()
    }

    def removeWeapon(index: Int): Unit =
    {
        _weapons.remove(index)
        updateWeaponList()
    }

    def item(hash: Int): Item =
        inventory.find(_.## == hash).get

    def addItem(item: Item): Unit =
    {
        inventory += item
        updateInventory()
    }

    def replaceItem(index: Int, item: Item): Unit =
    {
        inventory(index) = item
        updateInventory()
    }

    def replaceItemByHash(hash: Int, item: Item): Unit =
    {
        inventory(inventory.find(_.## == hash).map(i => inventory.indexOf(i)).get) = item
        updateInventory()
    }

    def removeItem(hash: Int): Unit =
    {
        inventory.remove(inventory.find(_.## == hash).map(i => inventory.indexOf(i)).get)
        updateInventory()
    }

    /** Returns the saved information as JSON string to be saved or exported. */
    def toJSON: String =
        JSON.stringify(MutableMap(
            "general" -> _general,
            "attributes" -> attributes.toJSDictionary,
            "status" -> _status.toJSDictionary,
            "customStatus" -> _customStatus.map(t => t._1 -> t._2.asInstanceOf[Dictionary[Int]]).toJSDictionary,
            "proficiency" -> proficiency.toJSArray,
            "expertise" -> expertise.toJSArray,
            "weapons" -> _weapons.toJSArray,
            "inventory" -> inventory.toJSArray,
            "notes" -> notes.toJSArray
        ).toJSDictionary)

    object score
    {
        def apply(attribute: String | Int): Int = attribute.asInstanceOf[Any] match
        {
            case s: String => attributes(s.toLowerCase.substring(0, 3))
            case i: Int => attributes.toList(i)._2
        }

        def update(attribute: String, value: Int): Unit =
        {
            attribute.asInstanceOf[Any] match
            {
                case s: String => attributes(s.toLowerCase.substring(0, 3)) = value
            }
            updateAbilities()
            updateAttributes()
            updateWeaponList()
        }
    }

}

object MainInformation
{
    def fromJSON(json: String): MainInformation =
    {
        val data = JSON.parse(json).asInstanceOf[Dictionary[Any]]
        new MainInformation(
            data("general").asInstanceOf[General],
            data("attributes").asInstanceOf[Dictionary[Int]],
            data("status").asInstanceOf[Dictionary[Int]],
            data("customStatus").asInstanceOf[Dictionary[Dictionary[Int]]].map(t => t._1 -> t._2.asInstanceOf[CustomStat]),
            data("proficiency").asInstanceOf[js.Array[String]].to(MutableSet),
            data("expertise").asInstanceOf[js.Array[String]].to(MutableSet),
            data("weapons").asInstanceOf[js.Array[Weapon]].to(ListBuffer),
            data("inventory").asInstanceOf[js.Array[Item]].to(ListBuffer),
            data("notes").asInstanceOf[js.Array[Note]].to(ListBuffer)
        )
    }
}

class General(var name: String = "Name",
              var `class`: String = "Fighter",
              var race: String = "Cyber Elf",
              var level: Int = 1,
              var experience: Int = 0) extends js.Object

class Weapon(val name: String,
             val die: String,
             val dieCount: Int,
             val melee: Boolean,
             val damageType: String,
             val notes: String,
             val hitBonus: Int,
             val damageBonus: Int,
             val proficiency: Boolean,
             val finesse: Boolean,
             val heavy: Boolean,
             val reach: Boolean,
             val loading: Boolean,
             val light: Boolean,
             val versatile: Boolean,
             val twoHanded: Boolean,
             val thrown: Boolean,
             val shortRange: Int,
             val longRange: Int) extends js.Object

class CustomStat(var max: Int,
                 var current: Int) extends js.Object

class Item(val name: String,
           val amount: String,
           val price: Int,
           val priceUnit: String,
           val weight: Double,
           val notes: String) extends js.Object

class Note(val title: String,
           val text: String) extends js.Object

object Extension
{

    implicit class ExtendedWeapon(weapon: Weapon)
    {
        def usedAttribute: String =
            if (weapon.melee && (!weapon.finesse || info.score("str") > info.score("dex")))
                "str"
            else
                "dex"

        def usedScore: Int =
            info.score(usedAttribute)

        def usedModifier: Int =
            statToModifier(usedScore)

        def toHit: Int =
            usedModifier + (if (weapon.proficiency) info.proficiencyBonus else 0) + weapon.hitBonus

        def toDamage: Int =
            usedModifier + weapon.damageBonus

        def damageString: String =
            s"${if (weapon.dieCount > 1) weapon.dieCount.toString else ""}${weapon.die}${
                if (weapon.versatile) "/" + dice(dice.indexOf(weapon.die) + 1) else ""
            } ${
                if (toDamage != 0)
                    f"$toDamage%+d"
                else
                    ""
            }"

        def rangeString: String =
            if (!weapon.melee || weapon.thrown)
                s"${weapon.shortRange} / ${weapon.longRange}"
            else "-"
    }

}