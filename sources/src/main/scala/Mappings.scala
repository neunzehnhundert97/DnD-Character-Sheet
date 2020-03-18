object Mappings
{
    /** A mapping from abilities to stats. */
    val abilities: Map[String, String] = Map(
        "Acrobatics" -> "dex",
        "Animal Handling" -> "wis",
        "Arcana" -> "int",
        "Athletics" -> "str",
        "Deception" -> "cha",
        "History" -> "int",
        "Insight" -> "wis",
        "Intimidation" -> "cha",
        "Investigation" -> "int",
        "Medicine" -> "wis",
        "Nature" -> "int",
        "Perception" -> "wis",
        "Performance" -> "cha",
        "Persuasion" -> "cha",
        "Religion" -> "int",
        "Sleight of Hand" -> "dex",
        "Stealth" -> "dex",
        "Survival" -> "wis",
    )

    /** A mapping from levels to the needed experience and proficiency bonus. */
    val levels: Map[Int, (Int, Int)] = Map(
        1 -> (0, 2),
        2 -> (300, 2),
        3 -> (900, 2),
        4 -> (2700, 2),
        5 -> (6500, 3),
        6 -> (14000, 3),
        7 -> (23000, 3),
        8 -> (34000, 3),
        9 -> (48000, 4),
        10 -> (64000, 4),
        11 -> (85000, 4),
        12 -> (100000, 4),
        13 -> (120000, 5),
        14 -> (140000, 5),
        15 -> (165000, 5),
        16 -> (195000, 5),
        17 -> (225000, 6),
        18 -> (265000, 6),
        19 -> (305000, 6),
        20 -> (355000, 6),
    )

    /** A mapping for each currency's value in cp. */
    val currencies: Map[String, Int] = Map(
        "cp" -> 1,
        "sp" -> 10,
        "ep" -> 50,
        "gp" -> 100,
        "pp" -> 200
    )
}
