module Battle exposing (getRandomNumberFromRange, Formation(..), Element(..), getDamage, terraAttacker, lockeTarget, terraStats, playableTerra, lockeStats, playableLocke, fireSpell, dirk, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)

import Random
import Task

perfectHitRate : Int
perfectHitRate =
    255

getRandomNumberFromRange : Random.Seed -> Int -> Int -> (Int, Random.Seed)
getRandomNumberFromRange seed start end =
    Random.step (Random.int start end) seed

type Attacker
    = CharacterAttacker PlayableCharacter
    | MonsterAttacker PlayingMonster

type Target
    = CharacterTarget PlayableCharacter
    | MonsterTarget PlayingMonster

-- Step 1

getDamage : Random.Seed -> Formation -> Attack -> Element -> List ElementAffected -> WeaponStats -> Attacker -> Target -> (Int, Random.Seed)
getDamage seed formation attack element elementsAffected weaponStats attacker target =
    getDamageStep1AMaximumDamage attack attacker (Damage 0)
    |> getStep21Step1PhysicalAttackDamage attack attacker
    |> getStep21Step2Damage attack attacker
    |> getDamageStep3 attack
    |> getDamageStep4 attack attacker
    |> getStep5Damage seed attack attacker
    |> (\(step5Damage, newStep5Seed) -> getStep6aDamageModificationsVariance newStep5Seed step5Damage)
    |> (\(damage, newSeed) -> (getStep6bDefenseModificationTarget attack target damage, newSeed) )
    |> (\(damage, newSeed) -> (getStep6cSafeShellDamage attack target damage, newSeed) )
    |> (\(damage, newSeed) -> (getStep6dDefendingModification attack target damage, newSeed) )
    |> (\(damage, newSeed) -> (getStep6eTargetBackRowModification attack target damage, newSeed))
    |> (\(damage, newSeed) -> (getStep6fTargetMorphModification attack target damage, newSeed))
    |> (\(damage, newSeed) -> (getStep6gCharacterOnCharacterOrHealingAttack attack attacker target damage, newSeed))
    |> (\(damage, newSeed) -> (getStep7Damage formation attack damage, newSeed))
    |> (\(damage, newSeed) -> (getDamageStep8 target damage, newSeed))
    |> (\(damage, newSeed) -> (getStep9Elements target element elementsAffected damage, newSeed))
    |> (\(damage, newSeed) -> case damage of
        Damage dam ->
            (dam, newSeed))

getHit : Random.Seed -> Attack -> AttackMissesDeathProtectedTargets -> Attacker -> Target -> HitResult
getHit seed attack attackMissesDeathProtectedTargets attacker target =
    getHitStep1attackAgainstClearTarget attack target
    |> Result.andThen (\_ -> getHitStep2DeathProtection target attackMissesDeathProtectedTargets)
    |> Result.andThen (\_ -> magicalAttackSpellUnblockable attack)
    |> (\ result ->
        case result of
            Err hitResult ->
                hitResult
            Ok _ ->
                Miss
    )


type Attack
    = PlayerPhysicalAttack
    | PlayerPhysicalMultipleAttack
    | PlayerMagicalAttack Spell
    | PlayerMagicalMultipleAttack Spell
    | PlayerHealingAttack
    | MonsterPhysicalAttack
    | MonsterPhysicalMultipleAttack
    | MonsterMagicalAttack Spell
    | MonsterMagicalMultipleAttack Spell
    | Special SpecialAttack


type SpecialAttack
    = BREAK
    | DOOM
    | DEMI
    | QUARTR
    | X_ZONE
    | W_WIND
    | SHOAT
    | ODIN
    | RAIDEN
    | ANTLION
    | SNARE
    | X_FER
    | GRAV_BOMB

type alias Preemptive = Bool

type Formation
    = NormalFormation Preemptive
    | BackFormation
    | PincerFormation
    | SideFormation Preemptive

type Damage = Damage Int
type SpellPower = SpellPower Int
type Level = Level Int
type MagicPower = MagicPower Int
type Physical = Physical Bool
type IgnoresDefense = IgnoresDefense Bool
type Unblockable = Unblockable Bool
type HitRate = HitRate Int  

type alias Spell =
    { magicType : MagicType
    , power : SpellPower
    , physical : Physical
    , ignoresDefense : IgnoresDefense
    , unblockable : Unblockable
    , hitRate : HitRate }

type MagicType
    = BlackMagic
    | GrayMagic
    | WhiteMagic
    | EsperMagic
    | SwdTech
    | Blitz
    | Dance
    | Lore
    | Magitek
    | Desparation
    | Miscellaneous

fireSpell : Spell
fireSpell =
    { magicType = BlackMagic
    , power = SpellPower 21
    , physical = Physical False
    , ignoresDefense = IgnoresDefense False
    , unblockable = Unblockable False
    , hitRate = HitRate 150 }

type BattlePower = BattlePower Int
type Evade = Evade Int
type MBlock = MBlock Int
type Speed = Speed Int
type Stamina = Stamina Int
type Vigor = Vigor Int
type MagicDefense = MagicDefense Int

type Character
    = Terra
    | Locke 
    | Cyan
    | Shadow 
    | Edgar 
    | Sabin
    | Celes
    | Strago
    | Relm 
    | Setzer 
    | Mog
    | Gau
    | Gogo
    | Umaro

type alias CharacterStats =
    { character : Character
    , level : Level
    , vigor : Vigor
    , speed : Speed 
    , stamina : Stamina 
    , magicPower : MagicPower 
    , battlePower : BattlePower 
    , defense : Defense 
    , magicDefense : MagicDefense
    , mblock : MBlock 
    , evade : Evade }

type Item
    = Antidote
    | BoldEdge
    | DriedMeat

type alias ItemStats =
    { name : String
    , type_ : Item
    , price : Price 
    , description : String }

type Element
    = NotElement
    | Water
    | Fire
    | Lightning
    | Poison
    | Ice
    | Pearl
    | Wind
    | Earth


type ElementEffect
    = ElementHasBeenNullified
    | TargetAbsorbsElement
    | TargetIsImmuneToElement
    | TargetIsResistantToElement
    | TargetIsWeakToElement


type alias ElementAffected =
    { element : Element, effect : ElementEffect }

type Monster
    = Abolisher
    | Actaneon
    | Adamanchyt

type alias MonsterStats =
    { type_ : Monster
    , level : Level
    , hitPoints : HitPoints
    , magicPoints : MagicPoints
    , xp : XP
    , gold : Gold
    , battlePower : BattlePower
    , vigor : Vigor
    , hitRate : HitRate
    , defense : Defense
    , evade : Evade
    , magicPower : MagicPower
    , speed : Speed 
    , magicDefense : MagicDefense
    , mblock : MBlock
    , stolenItems : List Item
    , droppedItems : List Item
    , absorbs : List Element
    , noEffect : List Element
    , weak : List Element }

-- Monster's don't have a row position, but it makes it easier
-- if we treat them like a character through the Damage functions,
-- so for now I'll just assume all monsters are Front.
type alias PlayingMonster =
    { stats : MonsterStats
    , hasSafeStatus : HasSafeStatus
    , hasShellStatus : HasShellStatus
    , hasPetrifyStatus : HasPetrifyStatus
    , hasClearStatus : HasClearStatus
    , protectedFromWound : ProtectedFromWound
    , rowPosition : RowPosition }

type Gold = Gold Int
type HitPoints = HitPoints Int
type MagicPoints = MagicPoints Int
type XP = XP Int

type alias PlayableCharacter =
    { stats : CharacterStats
    , equippedRelics : EquippedRelics
    , equippedWeapons : EquippedWeapons
    , hasSafeStatus : HasSafeStatus
    , hasShellStatus : HasShellStatus
    , hasMorphStatus : HasMorphStatus
    , hasBerserkStatus : HasBerserkStatus
    , hasPetrifyStatus : HasPetrifyStatus
    , hasClearStatus : HasClearStatus
    , protectedFromWound : ProtectedFromWound
    , defending : Defending
    , rowPosition : RowPosition
    , absorbs : List Element
    , noEffect : List Element
    , weak : List Element
    , resistant : List Element }

terraStats : CharacterStats
terraStats =
    { character = Terra
    , level = Level 1
    , vigor = Vigor 31
    , speed = Speed 33
    , stamina = Stamina 28
    , magicPower = MagicPower 39
    , battlePower = BattlePower 12
    , defense = Defense 42
    , magicDefense = MagicDefense 33
    , mblock = MBlock 7
    , evade = Evade 5 }

playableTerra : PlayableCharacter 
playableTerra =
    { stats = terraStats
    , equippedRelics = { leftHand = Just Earring, rightHand = Just Earring }
    , equippedWeapons = { leftHand = Nothing, rightHand = Just (EquippedWeapon mithrilKnife) }
    , hasSafeStatus = HasSafeStatus False 
    , hasShellStatus = HasShellStatus False
    , hasMorphStatus = HasMorphStatus False
    , hasBerserkStatus = HasBerserkStatus False 
    , hasPetrifyStatus = HasPetrifyStatus False
    , hasClearStatus = HasClearStatus False
    , protectedFromWound = ProtectedFromWound False
    , defending = Defending False
    , rowPosition = Front
    , absorbs = []
    , noEffect = []
    , weak = []
    , resistant = [] }

terraAttacker : Attacker
terraAttacker =
    CharacterAttacker playableTerra

lockeStats : CharacterStats
lockeStats =
    { character = Locke
    , level = Level 1
    , vigor = Vigor 37
    , speed = Speed 40
    , stamina = Stamina 31
    , magicPower = MagicPower 28
    , battlePower = BattlePower 14
    , defense = Defense 46
    , magicDefense = MagicDefense 23
    , mblock = MBlock 2
    , evade = Evade 15 }

playableLocke : PlayableCharacter
playableLocke =
    { stats = lockeStats
    , equippedRelics = { leftHand = Just HeroRing, rightHand = Just SneakRing }
    , equippedWeapons = { leftHand = Nothing, rightHand = Just (EquippedWeapon dirk) }
    , hasSafeStatus = HasSafeStatus False 
    , hasShellStatus = HasShellStatus False
    , hasMorphStatus = HasMorphStatus False
    , hasBerserkStatus = HasBerserkStatus False
    , hasPetrifyStatus = HasPetrifyStatus False
    , hasClearStatus = HasClearStatus False
    , protectedFromWound = ProtectedFromWound False
    , defending = Defending False
    , rowPosition = Front
    , absorbs = []
    , noEffect = []
    , weak = []
    , resistant = [] }

lockeTarget : Target
lockeTarget =
    CharacterTarget playableLocke


-- 2.1 - Step 1 magical attacks made by characters

getSpellFromMagicalAttack : Attack -> Maybe Spell
getSpellFromMagicalAttack attack =
    case attack of
        PlayerMagicalAttack spell ->
            Just spell
        PlayerMagicalMultipleAttack spell ->
            Just spell
        MonsterMagicalAttack spell ->
            Just spell
        MonsterMagicalMultipleAttack spell ->
            Just spell
        _ ->
            Nothing

getSpellPower : SpellPower -> Int
getSpellPower (SpellPower spellPower) =
    spellPower

getDamageStep1ACharacters : Attack -> MagicPower -> Level -> Damage -> Damage
getDamageStep1ACharacters attack (MagicPower magicPower) (Level level) (Damage damage) =
    case getSpellFromMagicalAttack attack of
        Just spell ->
            Damage ((getSpellPower spell.power) * 4 + (level * magicPower * (getSpellPower spell.power) // 32) + damage)
        Nothing ->
            Damage damage

-- 2.1 - Step 1 magical attacks made by monsters
getDamageStep1AMonsters : Attack -> MagicPower -> Level -> Damage -> Damage
getDamageStep1AMonsters attack (MagicPower magicPower) (Level level) (Damage damage) =
    case getSpellFromMagicalAttack attack of
        Just spell ->
            Damage ((getSpellPower spell.power) * 4 + (level * magicPower * 3 // 2) * (getSpellPower spell.power) // 32)
        Nothing ->
            Damage damage

getDamageStep1AMaximumDamage : Attack -> Attacker -> Damage -> Damage
getDamageStep1AMaximumDamage attack attacker damage =
    case attacker of
        CharacterAttacker playChar ->
            getDamageStep1ACharacters attack playChar.stats.magicPower playChar.stats.level damage
        MonsterAttacker playMon ->
            getDamageStep1AMonsters attack playMon.stats.magicPower playMon.stats.level damage




type Relic
    = AtlasArmlet
    | BlackBelt
    | CharmBangle
    | CursedRing
    | DragonHorn
    | Earring
    | Gauntlet
    | GenjiGlove
    | HeroRing
    | HyperWrist
    | MarvelShoes
    | Offering
    | RelicRing
    | Ribbon
    | SneakRing
    | SniperSight
    | Tintinabar
    | TrueKnight

type alias EquippedRelics =
    { leftHand : Maybe Relic
    , rightHand : Maybe Relic }

type Weapon
    = AirLancet
    | Assassin
    | Dirk
    | Graedus
    | Guardian
    | ManEater
    | MithrilKnife
    | SwordBreaker
    | ThiefKnife
    | ValiantKnife
    | AtmaWeapon
    | Blizzard
    | BreakBlade
    | Crystal
    | Drainer
    | Enhancer
    | Epee
    | Excalibur
    | Falchion
    | FlameSabre
    | Illumina
    | MithrilBlade
    | OgreNix
    | Ragnarok
    | RegalCutlass
    | RuneEdge
    | Scimitar
    | SoulSabre
    | ThunderBlade
    | AuraLance
    | GoldLance
    | ImpHalberd
    | MithrilPike
    | Partisan
    | PearlLance
    | StoutSpear
    | Trident
    | Blossom
    | Hardened
    | Imperial
    | Kodachi
    | Striker
    | Stunner
    | Ashura
    | Aura
    | Forged
    | Kotetsu
    | Murasame
    | SkyRender
    | Strato
    | Tempest
    | FireRod
    | GravityRod
    | HealRod
    | IceRod
    | MagusRod
    | MithrilRod
    | PearlRod
    | PoisonRod
    | Punisher
    | ThunderRod
    | ChocoboBrsh
    | DaVinciBrsh
    | MagicalBrsh
    | RainbowBrsh
    | NinjaStar
    | Shuriken
    | TackStar
    | BoneClub
    | Boomerang
    | Flail
    | FullMoon
    | HawkEye
    | MorningStar
    | RisingSun
    | Sniper
    | WingEdge
    | Cards
    | Darts
    | Dice
    | DoomDarts
    | FixedDice
    | Trump
    | DragonClaw
    | FireKnuckle
    | Kaiser
    | MetalKnuckle
    | MithrilClaw
    | PoisonClaw
    | TigerFangs

type alias WeaponStats =
    { weapon : Weapon
    , power : BattlePower
    , hitRate : HitRate 
    , price : Price 
    , equippableCharacters : List Character }

dirk : WeaponStats
dirk =
    { weapon = Dirk
    , power = BattlePower 26
    , hitRate = HitRate 180
    , price = Price 150 
    , equippableCharacters = [ Terra, Locke, Shadow, Edgar, Celes, Strago, Relm, Setzer, Mog, Gogo ]}

mithrilKnife : WeaponStats
mithrilKnife =
    { weapon = MithrilKnife
    , power = BattlePower 30
    , hitRate = HitRate 180
    , price = Price 300
    , equippableCharacters = [ Terra, Locke, Shadow, Edgar, Celes, Strago, Relm, Setzer, Mog, Gogo ] }

type Armor
    = AegisShld
    | Buckler
    | CrystalShld
    | CursedShld
    | DiamondShld
    | FlameShld
    | ForceShld
    | GenjiShld
    | GoldShld
    | HeavyShld
    | IceShld
    | MithrilShld
    | PaladinShld
    | ThunderShld
    | TortoiseShld
    | Bandana
    | BardsHat
    | Beret
    | CatHood
    | Circlet
    | Coronet
    | CrystalHelm
    | DarkHood
    | DiamondHelm
    | GenjiHelmet
    | GoldHelmet
    | GreenBeret
    | HairBand
    | HeadBand
    | IronHelmet
    | LeatherHat
    | MagusHat
    | MithrilHelm
    | MysteryVeil
    | OathVeil
    | PlumedHat
    | RedCap
    | RegalCrown
    | Thornlet
    | Tiara
    | TigerMask
    | Titanium
    | BehemothSuit
    | ChocoboSuit
    | CottonRobe
    | CrystalMail
    | CzarinaGown
    | DarkGear
    | DiamondVest
    | DiamondArmor
    | ForceArmor
    | GaiaGear
    | GenjiArmor
    | GoldArmor
    | ImpsArmor
    | IronArmor
    | KungFuSuit
    | LeatherArmor
    | LightRobe
    | Minerva
    | MirageVest
    | MithrilMail
    | MithrilVest
    | MoogleSuit
    | NinjaGear
    | NutkinSuit
    | PowerSash
    | RedJacket
    | SilkRobe
    | SnowMuffler
    | TabbySuit
    | TaoRobe
    | WhiteDress

type WearableArmorType
    = Shield
    | Helmet
    | BodyArmor

type alias ArmorStats =
    { armor : Armor
    , type_ : WearableArmorType
    , defense : Defense 
    , magicDefense : MagicDefense
    , price : Price
    , equippableCharacters : List Character }

type alias ShieldStats =
    { armor : Armor
    , type_ : WearableArmorType
    , defense : Defense 
    , magicDefense : MagicDefense
    , price : Price
    , equippableCharacters : List Character }

buckler : ShieldStats
buckler =
    { armor = Buckler
    , type_ = Shield
    , defense = Defense 16
    , magicDefense = MagicDefense 10
    , price = Price 200
    , equippableCharacters = [Terra, Locke, Cyan, Shadow, Edgar, Sabin, Celes, Strago, Relm, Setzer, Mog, Gau, Gogo] }

leatherHat : ArmorStats
leatherHat =
    { armor = LeatherHat
    , type_ = Helmet 
    , defense = Defense 11
    , magicDefense = MagicDefense 7
    , price = Price 50 
    , equippableCharacters = [Terra, Locke, Cyan, Shadow, Edgar, Sabin, Celes, Strago, Relm, Setzer, Mog, Gau, Gogo]}

leatherArmor : ArmorStats
leatherArmor =
    { armor = LeatherArmor
    , type_ = BodyArmor
    , defense = Defense 28
    , magicDefense = MagicDefense 19
    , price = Price 150
    , equippableCharacters = [Terra, Locke, Cyan, Shadow, Edgar, Celes, Strago, Relm, Setzer, Mog, Gau, Gogo]}

type Price = Price Int


type Equippable
    = EquippedWeapon WeaponStats
    | EquippedShield ShieldStats

type alias EquippedWeapons =
    { leftHand : Maybe Equippable, rightHand: Maybe Equippable }


-- 2.1 Step 2b
getEquippedWithOneEarring : EquippedRelics -> Bool
getEquippedWithOneEarring { leftHand, rightHand } =
    if leftHand == Just Earring && rightHand /= Just Earring then
        True
    else if rightHand == Just Earring && leftHand /= Just Earring then
        True
    else
        False

getEquippedWithOneHeroRing : EquippedRelics -> Bool
getEquippedWithOneHeroRing { leftHand, rightHand } =
    if leftHand == Just HeroRing && rightHand /= Just HeroRing then
        True
    else if rightHand == Just HeroRing && leftHand /= Just HeroRing then
        True
    else
        False

-- 2.1 Step 2c
getEquippedWithTwoEarrings : EquippedRelics -> Bool
getEquippedWithTwoEarrings { leftHand, rightHand } =
    if leftHand == Just Earring && rightHand == Just Earring then
        True
    else
        False

getEquippedWithTwoHeroRings : EquippedRelics -> Bool
getEquippedWithTwoHeroRings { leftHand, rightHand } =
    if leftHand == Just HeroRing && rightHand == Just HeroRing then
        True
    else
        False

-- -- magical attacks made by monsters
-- getDamageStep1B : Attack -> SpellPower -> MagicPower -> Level -> Damage -> Damage
-- getDamageStep1B attack (SpellPower spellPower) (MagicPower magicPower) (Level level) (Damage damage) =
--     case attack of
--         MonsterMagicalAttack ->
--             Damage (spellPower * 4 + (level * (magicPower * 3 // 2) * spellPower // 32) + damage)

--         _ ->
--             Damage damage



-- 2.1 - Step 1a

getStep1aPhysicalAttack : Vigor -> Vigor
getStep1aPhysicalAttack (Vigor vigor) =
    let
        vigor2 = vigor * 2
    in
    if vigor >= 128 then
        Vigor 255

    else
        Vigor vigor2

-- 2.1 - Step 1b

type AttackPower = AttackPower Int

getStep1bAttackPower : BattlePower -> Vigor -> AttackPower 
getStep1bAttackPower  (BattlePower power) (Vigor vigor) =
    AttackPower (power + vigor)

-- 2.1 - Step 1c

getStep1cGauntletIncreasedAttack : BattlePower -> EquippedRelics -> AttackPower -> AttackPower
getStep1cGauntletIncreasedAttack (BattlePower battlePower) equippedRelics (AttackPower attackPower) =
    if equippedRelics.leftHand == Just Gauntlet || equippedRelics.rightHand == Just Gauntlet then
        attackPower + battlePower * 3 // 4 |> AttackPower
    else
        AttackPower attackPower

-- 2.1 - Step 1d

getDamageStep1dBasicDamage : BattlePower -> Level -> AttackPower -> Damage
getDamageStep1dBasicDamage (BattlePower battlePower) (Level level) (AttackPower attackPower) =
    battlePower + ((level * level * attackPower) // 256) * 3 // 2 |> Damage

-- 2.1 - Step 1e

getStep1eOfferingDecreasedDamage : EquippedRelics -> Damage -> Damage
getStep1eOfferingDecreasedDamage equippedRelics (Damage damage) =
    if equippedRelics.leftHand == Just Offering || equippedRelics.rightHand == Just Offering then
        damage // 2 |> Damage
    else
        Damage damage

-- 2.1 - Step 1f

equippedWithGenjiGlove : EquippedRelics -> Bool
equippedWithGenjiGlove equippedRelics =
    if equippedRelics.leftHand == Just GenjiGlove || equippedRelics.rightHand == Just GenjiGlove then
        True
    else
        False

type EquippedWeaponCount
    = ZeroWeapons
    | OneWeapon
    | TwoWeapons

hasEquippedWeapon : Maybe Equippable -> Bool
hasEquippedWeapon maybeEquippable =
    case maybeEquippable of
        Just equippable ->
            case equippable of
                EquippedWeapon _ ->
                    True
                EquippedShield _ ->
                    False
        Nothing ->
            False

getEquippedWeaponCount : EquippedWeapons -> EquippedWeaponCount
getEquippedWeaponCount equippedWeapons =
    if hasEquippedWeapon equippedWeapons.leftHand && hasEquippedWeapon equippedWeapons.rightHand then
        TwoWeapons
    else if hasEquippedWeapon equippedWeapons.leftHand && hasEquippedWeapon equippedWeapons.rightHand == False then
        OneWeapon
    else if hasEquippedWeapon equippedWeapons.leftHand == False && hasEquippedWeapon equippedWeapons.rightHand then
        OneWeapon
    else
        ZeroWeapons 

getStep1fDamageFromGenjiGlove : Attack -> EquippedRelics -> EquippedWeapons -> Damage -> Damage
getStep1fDamageFromGenjiGlove attack equippedRelics equippedWeapons (Damage damage) =
    if isPhysicalAttack attack && equippedWithGenjiGlove equippedRelics == True && getEquippedWeaponCount equippedWeapons /= TwoWeapons then
        Basics.ceiling ( (toFloat damage) * (3 / 4)) |> Damage
    else
        Damage damage 

get21Step1PhysicalAttackDamageByCharacters : Attack -> PlayableCharacter -> Damage -> Damage
get21Step1PhysicalAttackDamageByCharacters attack playChar damage =
    getStep1aPhysicalAttack playChar.stats.vigor
    |> getStep1bAttackPower playChar.stats.battlePower
    |> getStep1cGauntletIncreasedAttack playChar.stats.battlePower playChar.equippedRelics
    |> getDamageStep1dBasicDamage playChar.stats.battlePower playChar.stats.level
    |> getStep1eOfferingDecreasedDamage playChar.equippedRelics
    |> getStep1fDamageFromGenjiGlove attack playChar.equippedRelics playChar.equippedWeapons


getStep1aMonsterPhysicalAttackDamage : Attack -> Vigor -> BattlePower -> Level -> Damage -> Damage
getStep1aMonsterPhysicalAttackDamage attack (Vigor vigor) (BattlePower battlePower) (Level level) (Damage damage) =
    if isMonsterPhysicalAttack attack then
        level * level * (battlePower * 4 + vigor) // 256 + damage |> Damage
    else
        Damage damage

getStep21Step1PhysicalAttackDamage : Attack -> Attacker -> Damage -> Damage
getStep21Step1PhysicalAttackDamage attack attacker damage =
    case attacker of
        CharacterAttacker playChar ->
            get21Step1PhysicalAttackDamageByCharacters attack playChar damage
        MonsterAttacker playMon ->
            getStep1aMonsterPhysicalAttackDamage attack playMon.stats.vigor playMon.stats.battlePower playMon.stats.level damage



-- -- getRandomMonsterVigor : Seed -> Int
-- -- getRandomMonsterVigor seed =
-- --     Tuple.first (getRandomNumberFromRange 56 63 seed)



-- 2.1 - Step 2a

hasEquippedRelic : EquippedRelics -> Relic -> Bool
hasEquippedRelic equippedRelics relic =
    if equippedRelics.leftHand == Just relic || equippedRelics.rightHand == Just relic then
        True
    else
        False

getStep2aPhysicalAtlasArmletOrHeroRing : Attack -> EquippedRelics -> Damage -> Damage
getStep2aPhysicalAtlasArmletOrHeroRing attack relics (Damage damage) =
    if isPhysicalAttack attack && (hasEquippedRelic relics AtlasArmlet || hasEquippedRelic relics HeroRing) then
        damage * 5 // 4 |> Damage
    else
        Damage damage 

-- 2.1 - Step 2b

hasOnly1EquippedRelic : EquippedRelics -> Relic -> Bool
hasOnly1EquippedRelic equippedRelics relic =
    if equippedRelics.leftHand == Just relic && equippedRelics.rightHand /= Just relic then
        True
    else if equippedRelics.leftHand /= Just relic && equippedRelics.rightHand == Just relic then
        True
    else
        False
 

getStep2bEarringOrHeroRing : Attack -> EquippedRelics -> Damage -> Damage
getStep2bEarringOrHeroRing attack relics (Damage damage) =
    if isMagicalAttack attack && (hasOnly1EquippedRelic relics Earring || hasOnly1EquippedRelic relics HeroRing) then
        damage * 5 // 4 |> Damage
    else
        Damage damage

-- 2.1 - Step 2c

has2EquippedRelics : EquippedRelics -> Relic -> Bool
has2EquippedRelics equippedRelics relic =
    if equippedRelics.leftHand == Just relic && equippedRelics.rightHand == Just relic then
        True
    else
        False

getStep2c2EarringsOrHeroRings : Attack -> EquippedRelics -> Damage -> Damage
getStep2c2EarringsOrHeroRings attack relics (Damage damage) =
    if isMagicalAttack attack && (has2EquippedRelics relics Earring || has2EquippedRelics relics HeroRing) then
        damage + (damage // 4) + (damage //4) |> Damage
    else
        Damage damage

getStep21Step2Damage : Attack -> Attacker -> Damage -> Damage
getStep21Step2Damage attack attacker damage =
    case attacker of
        CharacterAttacker playChar ->
            getStep2aPhysicalAtlasArmletOrHeroRing attack playChar.equippedRelics damage
            |> getStep2bEarringOrHeroRing attack playChar.equippedRelics
            |> getStep2c2EarringsOrHeroRings attack playChar.equippedRelics
        MonsterAttacker _ ->
            damage


-- Step 3

getDamageStep3 : Attack -> Damage -> Damage
getDamageStep3 attack (Damage damage) =
    if isMagicalMultipleTargetAttack attack then
        damage // 2 |> Damage
    else
        Damage damage



-- Step 4

type RowPosition
    = Front
    | Back

getDamageStep4 : Attack -> Attacker -> Damage -> Damage
getDamageStep4 attack attacker (Damage damage) =
    case attacker of
        CharacterAttacker playChar ->        
            if isPhysicalAttack attack && playChar.rowPosition == Back then
                damage // 2 |> Damage

            else
                Damage damage
        MonsterAttacker _ ->
            Damage damage



-- Step 5

type CriticalHitDamageMultiplier = CriticalHitDamageMultiplier Int

startingCriticalHitMultiplier : CriticalHitDamageMultiplier
startingCriticalHitMultiplier =
    CriticalHitDamageMultiplier 0

type HasMorphStatus = HasMorphStatus Bool

getMorphStatusMultiplier : HasMorphStatus -> CriticalHitDamageMultiplier -> CriticalHitDamageMultiplier
getMorphStatusMultiplier (HasMorphStatus hasMorphStatus) (CriticalHitDamageMultiplier multiplier) =
    if hasMorphStatus == True then
        CriticalHitDamageMultiplier (multiplier + 2)

    else
        CriticalHitDamageMultiplier multiplier

type HasBerserkStatus = HasBerserkStatus Bool

getBerserkStatusAndPhysicalAttackMultiplier : Attack -> HasBerserkStatus -> CriticalHitDamageMultiplier -> CriticalHitDamageMultiplier
getBerserkStatusAndPhysicalAttackMultiplier attack (HasBerserkStatus hasBerserkStatus) (CriticalHitDamageMultiplier multiplier) =
    if hasBerserkStatus == True && isPhysicalAttack attack then
        CriticalHitDamageMultiplier (multiplier + 1)
    else
        CriticalHitDamageMultiplier multiplier


type CriticalHit = CriticalHit Bool

getCriticalHit : Random.Seed -> (CriticalHit, Random.Seed)
getCriticalHit seed =
    let
        (result, newSeed)  = getRandomNumberFromRange seed 1 32
        crit = CriticalHit (result == 32)
    in
    (crit, newSeed)


getCriticalHitMultiplier : CriticalHit -> CriticalHitDamageMultiplier -> CriticalHitDamageMultiplier
getCriticalHitMultiplier (CriticalHit criticalHit) (CriticalHitDamageMultiplier multiplier) =
    if criticalHit == True then
        CriticalHitDamageMultiplier (multiplier + 2)
    else
        CriticalHitDamageMultiplier multiplier

-- Step 5 - 5a
getStep5Damage : Random.Seed -> Attack -> Attacker -> Damage -> (Damage, Random.Seed)
getStep5Damage seed attack attacker (Damage damage) =
    case attacker of
        CharacterAttacker playChar ->
            let
                (crit, newSeed) = getCriticalHit seed
            in
            getMorphStatusMultiplier playChar.hasMorphStatus startingCriticalHitMultiplier
            |> getBerserkStatusAndPhysicalAttackMultiplier attack playChar.hasBerserkStatus
            |> getCriticalHitMultiplier crit
            |> (\(CriticalHitDamageMultiplier multiplier) -> (damage + ((damage // 2) * multiplier)))
            |> (\finalDamage -> (Damage finalDamage, newSeed))
        MonsterAttacker _ ->
            (Damage damage, seed)


    

-- Step 6a - random variance

getStep6aDamageModificationsVariance : Random.Seed -> Damage -> (Damage, Random.Seed)
getStep6aDamageModificationsVariance seed (Damage damage) =
    getRandomNumberFromRange seed 224 255
    |> (\(int, newSeed) -> ((damage * int // 256) + 1, newSeed) )
    |> (\ (newDamage, newSeed ) -> (Damage newDamage, newSeed))



-- 2.1 - Step 6b
-- TODO: figure out a way to use the bug by default, but turn it off
-- if the player wants to

type Defense = Defense Int

getPhysicalDefenseModification : Damage -> Defense -> Damage
getPhysicalDefenseModification (Damage damage) (Defense defense) =
    (damage * (256 - defense) // 256) + 1 |> Damage

getMagicalDefenseModification : Damage -> MagicDefense -> Damage 
getMagicalDefenseModification (Damage damage) (MagicDefense defense) =
    (damage * (256 - defense) // 256) + 1 |> Damage

getStep6bDefenseDamageModification : Attack -> Damage -> Defense -> MagicDefense -> Damage
getStep6bDefenseDamageModification attack damage def mblock =
    if isPhysicalAttack attack then
        getPhysicalDefenseModification damage def
    else if isMagicalAttack attack then
        getMagicalDefenseModification damage mblock
    else
        damage 

getStep6bDefenseModificationTarget : Attack -> Target -> Damage -> Damage
getStep6bDefenseModificationTarget attack target damage =
    case target of
        CharacterTarget playingCharacter ->
            getStep6bDefenseDamageModification attack damage playingCharacter.stats.defense playingCharacter.stats.magicDefense
        MonsterTarget playingMonster ->
            getStep6bDefenseDamageModification attack damage playingMonster.stats.defense playingMonster.stats.magicDefense
        
-- 2.1 - Step 6c

type HasSafeStatus = HasSafeStatus Bool

getPhysicalAttackAgainstSafeTarget : Attack -> HasSafeStatus -> Damage -> Damage
getPhysicalAttackAgainstSafeTarget attack (HasSafeStatus safeStatus) (Damage damage) =
    if isPhysicalAttack attack && safeStatus == True then
        (damage * 170 // 256) + 1 |> Damage
    else
        Damage damage

type HasShellStatus = HasShellStatus Bool 

getMagicalAttackAgainstShellTarget : Attack -> HasShellStatus -> Damage -> Damage
getMagicalAttackAgainstShellTarget attack (HasShellStatus shellStatus) (Damage damage) =
    if isMagicalAttack attack && shellStatus == True then
        (damage * 170 // 256) + 1 |> Damage
    else
        Damage damage 

getHasSafeStatusFromTarget : Target -> HasSafeStatus
getHasSafeStatusFromTarget target =
    case target of
        CharacterTarget pc ->
            pc.hasSafeStatus
        MonsterTarget mon ->
            mon.hasSafeStatus

getHasShellStatusFromTarget : Target -> HasShellStatus
getHasShellStatusFromTarget target =
    case target of
        CharacterTarget pc ->
            pc.hasShellStatus
        MonsterTarget mon ->
            mon.hasShellStatus
            
getStep6cSafeShellDamage : Attack -> Target -> Damage -> Damage
getStep6cSafeShellDamage attack target damage =
    getPhysicalAttackAgainstSafeTarget attack (getHasSafeStatusFromTarget target) damage
    |> getMagicalAttackAgainstShellTarget attack (getHasShellStatusFromTarget target)

-- Step 6d
type Defending = Defending Bool 

getIsDefending : PlayableCharacter -> Bool
getIsDefending playChar =
    case playChar.defending of
        Defending def ->
            def

getStep6dDefendingModification : Attack -> Target -> Damage -> Damage
getStep6dDefendingModification attack target (Damage damage) =
    case target of
        CharacterTarget playChar ->
            if isPhysicalAttack attack && (getIsDefending playChar) == True then
                damage // 2 |> Damage
            else
                Damage damage
        MonsterTarget _ ->
            Damage damage

-- Step 6e

getTargetsRow : Target -> RowPosition
getTargetsRow target =
    case target of
        CharacterTarget playChar ->
            playChar.rowPosition
        MonsterTarget playMon ->
            playMon.rowPosition

getStep6eTargetBackRowModification : Attack -> Target -> Damage -> Damage
getStep6eTargetBackRowModification attack target (Damage damage) =
    if isPhysicalAttack attack && (getTargetsRow target) == Back then
        damage // 2 |> Damage
    else
        Damage damage

-- Step 6f

getTargetMorphStatus : Target -> Bool
getTargetMorphStatus target =
    case target of
        CharacterTarget playChar ->
            case playChar.hasMorphStatus of
                HasMorphStatus hasIt ->
                    hasIt
        MonsterTarget _ ->
            False

getStep6fTargetMorphModification : Attack -> Target -> Damage -> Damage
getStep6fTargetMorphModification attack target (Damage damage) =
    if isMagicalAttack attack && (getTargetMorphStatus target) == True then
        damage // 2 |> Damage
    else
        Damage damage

attackerIsCharacter : Attacker -> Bool
attackerIsCharacter attacker =
    case attacker of
        CharacterAttacker _ ->
            True
        MonsterAttacker _ ->
            False

attackerIsMonster : Attacker -> Bool
attackerIsMonster attacker =
    case attacker of
        CharacterAttacker _ ->
            False
        MonsterAttacker _ ->
            True

targetIsCharacter : Target -> Bool
targetIsCharacter target =
    case target of
        CharacterTarget _ ->
            True
        MonsterTarget _ ->
            False

targetIsMonster : Target -> Bool
targetIsMonster target =
    case target of
        CharacterTarget _ ->
            False
        MonsterTarget _ ->
            True


-- 2.1 - Step 6g
getStep6gCharacterOnCharacterOrHealingAttack : Attack -> Attacker -> Target -> Damage -> Damage
getStep6gCharacterOnCharacterOrHealingAttack attack attacker target (Damage damage) =
    case attack of
        PlayerHealingAttack ->
            Damage damage
        _ ->
            if attackerIsCharacter attacker && targetIsCharacter target then
                damage // 2 |> Damage

            else
                Damage damage

-- Step 7

-- 2.1 - Step 7a

getStep7Damage : Formation -> Attack -> Damage -> Damage
getStep7Damage formation attack (Damage damage) =
    if isPhysicalAttack attack && formation == BackFormation then
        damage + ((damage // 2) * 1) |> Damage
    else
        Damage damage


-- Step 8

type HasPetrifyStatus = HasPetrifyStatus Bool

getPetrifyStatusFromTarget : Target -> Bool
getPetrifyStatusFromTarget target =
    case target of
        CharacterTarget playChar ->
            case playChar.hasPetrifyStatus of
                HasPetrifyStatus hasIt ->
                    hasIt
        MonsterTarget playMon ->
            case playMon.hasPetrifyStatus of
                HasPetrifyStatus hasIt ->
                    hasIt


getDamageStep8 : Target -> Damage -> Damage
getDamageStep8 target damage =
    if (getPetrifyStatusFromTarget target) == True then
        Damage 0

    else
        damage



-- Step 9


-- Step 9a

getStep9aElementNullified : ElementEffect -> Damage -> Result (ElementEffect, Damage) Damage
getStep9aElementNullified effect damage =
    if effect == ElementHasBeenNullified then
        Err (effect, Damage 0)
    else
        Ok damage

-- NOTE: for now, just negativeing the damage invert to indicate healing
-- best english evarrr
getStep9bAborbsElement : ElementEffect -> Damage -> Result (ElementEffect, Damage) Damage
getStep9bAborbsElement effect (Damage damage) =
    if effect == TargetAbsorbsElement then
        Err (effect, damage * -1 |> Damage)
    else
        Ok (Damage damage)

getStep9cImmuneElement : ElementEffect -> Damage -> Result (ElementEffect, Damage) Damage
getStep9cImmuneElement effect damage =
    if effect == TargetIsImmuneToElement then
        Err (effect, Damage 0)
    else
        Ok damage

getStep9dResistentElement : ElementEffect -> Damage -> Result (ElementEffect, Damage) Damage
getStep9dResistentElement effect (Damage damage) =
    if effect == TargetIsResistantToElement then
        Err (effect, damage // 2 |> Damage)
    else
        Ok (Damage damage)

getStep9eWeakElement : ElementEffect -> Damage -> Result (ElementEffect, Damage) Damage
getStep9eWeakElement effect (Damage damage) =
    if effect == TargetIsWeakToElement then
        Err (effect, damage * 2 |> Damage)
    else
        Ok (Damage damage)

elementEffectMatches : Element -> ElementEffect -> List ElementAffected -> Maybe ElementEffect
elementEffectMatches element_ effect_ elementsAffected =
    List.filter (\ { element, effect } -> element == element_) elementsAffected
    |> List.filter (\ { effect } -> effect == effect_ )
    |> List.head
    |> (\elementAffectedItemMaybe ->
            case elementAffectedItemMaybe of
                Nothing ->
                    Nothing
                Just elementAffectedItem ->
                    Just elementAffectedItem.effect
        )

getStep9Elements : Target -> Element -> List ElementAffected -> Damage -> Damage
getStep9Elements target element elementsAffected damage =
    case target of
        CharacterTarget playChar ->
            case elementEffectMatches element ElementHasBeenNullified elementsAffected of
                Nothing ->
                    damage
                Just effect ->
                    getStep9aElementNullified effect damage
                    |> Result.andThen (getStep9bAborbsElement effect)
                    |> Result.andThen (getStep9cImmuneElement effect)
                    |> Result.andThen (getStep9dResistentElement effect)
                    |> Result.andThen (getStep9eWeakElement effect)
                    |> (\ result ->
                        case result of
                            Err (effectAffectingDamage, finalDamage) ->
                                finalDamage
                            Ok finalDamage ->
                                finalDamage
                    )
        MonsterTarget playMon ->
            damage


-- Hit Determinism


-- -- getRandomHitOrMissvalue : Seed -> Int
-- -- getRandomHitOrMissvalue seed =
-- --     Tuple.first (getRandomNumberFromRange 0 99 seed)


-- -- getRandomStaminaHitOrMissValue : Seed -> Int
-- -- getRandomStaminaHitOrMissValue seed =
-- --     Tuple.first (getRandomNumberFromRange 0 127 seed)


-- -- getRandomImageStatusRemoval : Seed -> Int
-- -- getRandomImageStatusRemoval seed =
-- --     Tuple.first (getRandomNumberFromRange 0 3 seed)


-- -- TODO: figure out how to create this so the 1 and 4 are built in
-- type alias RandomRemoveImageStatus = { start : Int, end : Int, result : Int }

-- -- getRandomRemoveImageStatus : RandomRemoveImageStatus
-- -- getRandomRemoveImageStatus =
-- --     { start = 1, end = 4, result = Waiting }

-- getRemoveImageStatus : RandomRemoveImageStatus -> Bool
-- getRemoveImageStatus { result } =
--     -- let
--     --     (num, seed) = getRandomNumberFromRange 1 4 seed)
--     --     removed = num == 4
--     -- in
--     -- ( removed, Seed seed)
--     result == 4


-- -- getMonsterStamina : Seed -> Int
-- -- getMonsterStamina maxHitPoints =
-- --     (maxHitPoints // 512) + 16 |> clamp 0 40



-- -- Get Hit

-- type RemoveImageStatus = RemoveImageStatus Bool 


isPhysicalAttack : Attack -> Bool
isPhysicalAttack attack =
    case attack of
        PlayerPhysicalAttack ->
            True

        PlayerPhysicalMultipleAttack ->
            True

        MonsterPhysicalAttack ->
            True

        MonsterPhysicalMultipleAttack ->
            True

        _ ->
            False

isMonsterPhysicalAttack : Attack -> Bool
isMonsterPhysicalAttack attack =
    case attack of
        MonsterPhysicalAttack ->
            True
        MonsterPhysicalMultipleAttack ->
            True
        _ ->
            False

isMagicalAttack : Attack -> Bool
isMagicalAttack attack =
    case attack of
        MonsterMagicalAttack _ ->
            True

        MonsterMagicalMultipleAttack _ ->
            True

        PlayerMagicalAttack _ ->
            True

        PlayerMagicalMultipleAttack _ ->
            True

        _ ->
            False

isMagicalMultipleTargetAttack : Attack -> Bool
isMagicalMultipleTargetAttack attack =
    case attack of
        PlayerMagicalMultipleAttack _ ->
            True
        MonsterMagicalMultipleAttack _ ->
            True
        _ ->
            False


-- isSpecialAttack : Attack -> Bool
-- isSpecialAttack attack =
--     isPhysicalAttack attack == False && isMagicalAttack attack == False

-- 2.2 - Step 1a

type HitResult
    = Hit
    | HitAndRemoveImageStatus
    | Miss 
    | MissAndRemoveImageStatus

type HasClearStatus = HasClearStatus Bool 

getClearStatusFromTarget : Target -> Bool
getClearStatusFromTarget target =
    case target of
        CharacterTarget playChar ->
            case playChar.hasClearStatus of
                HasClearStatus hasIt ->
                    hasIt
        MonsterTarget playMon ->
            case playMon.hasClearStatus of
                HasClearStatus hasIt ->
                    hasIt



getHitStep1attackAgainstClearTarget : Attack -> Target -> Result HitResult ()
getHitStep1attackAgainstClearTarget attack target =
    if isPhysicalAttack attack && (getClearStatusFromTarget target) == True then
        Err Miss

    else if isMagicalAttack attack && (getClearStatusFromTarget target) == True then
        Err Hit

    else
        Ok ()

-- 2.2 Step 2

type ProtectedFromWound = ProtectedFromWound Bool
type AttackMissesDeathProtectedTargets = AttackMissesDeathProtectedTargets Bool

getTargetProtectedFromWound : Target -> Bool
getTargetProtectedFromWound target =
    case target of
        CharacterTarget playChar ->
            case playChar.protectedFromWound of
                ProtectedFromWound hasIt ->
                    hasIt
        MonsterTarget playMon ->
            case playMon.protectedFromWound of
                ProtectedFromWound hasIt ->
                    hasIt

getHitStep2DeathProtection : Target -> AttackMissesDeathProtectedTargets -> Result HitResult ()
getHitStep2DeathProtection target (AttackMissesDeathProtectedTargets attackMissesDeathProtectedTargets) =
    if (getTargetProtectedFromWound target) && attackMissesDeathProtectedTargets then
        Err Miss

    else
        Ok ()

-- 2.2 - Step 3

getIsUnblockable : Unblockable -> Bool
getIsUnblockable (Unblockable unblockable) =
    unblockable

-- TODO: handle Special as some of those are unblockable, but unsure
-- if spell, so this type may not matter later when we handle specials.
getSpellUnblockableFromAttack : Attack -> Bool
getSpellUnblockableFromAttack attack =
    case attack of
        PlayerMagicalAttack { unblockable } ->
            getIsUnblockable unblockable
        PlayerMagicalMultipleAttack { unblockable } ->
            getIsUnblockable unblockable
        MonsterMagicalAttack { unblockable } ->
            getIsUnblockable unblockable
        MonsterMagicalMultipleAttack { unblockable } ->
            getIsUnblockable unblockable
        _ ->
            False

magicalAttackSpellUnblockable : Attack -> Result HitResult ()
magicalAttackSpellUnblockable attack =
    if isMagicalAttack attack && (getSpellUnblockableFromAttack attack) then
        Err Hit

    else
        Ok ()

-- -- 2.2 Step 4a

-- -- No Special Attacks --
-- -- Skip this if attack can be blocked by Stamina

-- type HasSleepStatus = HasSleepStatus Bool
-- type HasFreezeStatus = HasFreezeStatus Bool
-- type HasStopStatus = HasStopStatus Bool   

-- attackUnmissableAgainstTarget : HasSleepStatus -> HasPetrifyStatus -> HasFreezeStatus -> HasStopStatus -> AttackResult
-- attackUnmissableAgainstTarget (HasSleepStatus targetHasSleepStatus) (HasPetrifyStatus targetHasPetrifyStatus) (HasFreezeStatus targetHasFreezeStatus) (HasStopStatus targetHasStopStatus) =
--     if targetHasSleepStatus || targetHasPetrifyStatus || targetHasFreezeStatus || targetHasStopStatus then
--         Hit

--     else
--         Unknown

-- -- 2.2 Step 4b

-- type BackOfTarget = BackOfTarget Bool

-- physicalAttackBack : Attack -> BackOfTarget -> AttackResult
-- physicalAttackBack attack (BackOfTarget backOfTarget) =
--     if isPhysicalAttack attack && backOfTarget then
--         Hit

--     else
--         Unknown

-- -- 2.2 Step 4c
-- type HitRate = HitRate Int

-- isPerfectHitRate : HitRate -> AttackResult
-- isPerfectHitRate (HitRate hitRate) =
--     if hitRate == perfectHitRate then
--         Hit

--     else
--         Unknown

-- -- 2.2 Step 4d
-- type TargetHasImageStatus = TargetHasImageStatus Bool

-- physicalAttackAgainstImageStatus : Attack -> TargetHasImageStatus -> RandomRemoveImageStatus -> AttackResult
-- physicalAttackAgainstImageStatus attack (TargetHasImageStatus targetHasImageStatus) randomResult =
--     if isPhysicalAttack attack && targetHasImageStatus then
--         if getRemoveImageStatus randomResult == True then
--             MissAndRemoveImageStatus
--         else
--             Miss 

--     else
--         Unknown

-- -- 2.2 Step 4e

-- type MBlock = MBlock Int
-- type BlockValue = BlockValue Int

-- getStep4eBaseBlockValueFromBlock : MBlock -> Int
-- getStep4eBaseBlockValueFromBlock (MBlock block) =
--     (255 - block * 2) + 1

-- getStep4eBlockValueClamp : BlockValue -> BlockValue
-- getStep4eBlockValueClamp (BlockValue value) =
--     if value > 255 then
--         BlockValue 255
--     else if value < 1 then
--         BlockValue 1
--     else
--         BlockValue value

-- -- getStep4eHit : HitRate -> BlockValue -> Seed -> AttackResult
-- -- getStep4eHit (HitRate rate) (BlockValue value) seed =
-- --     if ((rate * value) / 256) > (getRandomNumberFromRange 0 99 seed) then
-- --         Hit
-- --     else
-- --         Miss

-- -- Special Attacks --

-- shouldUseStep5HitCheck : Attack -> Bool
-- shouldUseStep5HitCheck attack =
--     case attack of
--         Special _ -> True
--         _ -> False

-- -- getStep5aChanceToHit : Seed -> HitRate -> MagicBlock -> BlockValue
-- -- getStep5aChanceToHit (HitRate rate) (MagicBlock value) =
-- --     let
-- --         blockValue =
-- --             (255 - value * 2) + 1
-- --         clampedBlockValue =
-- --             clamp 1 255 blockValue
-- --     in
-- --     if ((rate * clampedBlockValue) / 256) > getRandomNumberFromRange 0 99 seed then
-- --         Hit
-- --     else
-- --         Miss
    

-- -- getStep5bStaminaHitOrMiss : Attack -> Stamina -> Seed -> AttackResult -> AttackResult
-- -- getStep5bStaminaHitOrMiss attack (Stamina targetStamina) seed hitInStep5a =
-- --     if isSpecialAttack attack then
-- --         if targetStamina >= getRandomNumberFromRange 0 127 seed then
-- --             Miss

-- --         else if hitInStep5a == Hit || hitInStep5a == HitAndRemoveImageStatus then
-- --             hitInStep5a

-- --         else
-- --             Miss

-- --     else
-- --         hitInStep5a

-- -- hitDetermination : Attack -> AttackResult
-- -- hitDetermination attack =
