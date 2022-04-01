module Battle exposing (getRandomNumberFromRange, getDamage, terra, playableTerra, locke, playableLocke, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)

import Random
import Task

perfectHitRate : Int
perfectHitRate =
    255

getRandomNumberFromRange : Random.Seed -> Int -> Int -> (Int, Random.Seed)
getRandomNumberFromRange seed start end =
    Random.step (Random.int start end) seed


-- Step 1

getDamage : Random.Seed -> Attack -> SpellPower -> MagicPower -> Level -> PlayableCharacter -> PlayableCharacter -> Int
getDamage seed attack spellPower magicPower level attacker target =
    getDamageStep1A attack spellPower magicPower level (Damage 0)
    |> getStep2MagicalAttackRelicBonus attack (EquippedWithOneEarring (getEquippedWithOneEarring attacker.equippedRelics)) (EquippedWithOneHeroRing (getEquippedWithOneHeroRing attacker.equippedRelics))
    |> getStep2MagicalAttackRelicsBonus attack (EquippedWithTwoEarrings (getEquippedWithTwoEarrings attacker.equippedRelics)) (EquippedWithTwoHeroRings (getEquippedWithTwoHeroRings attacker.equippedRelics))
    |> getDamageStep3 attack
    |> getStep6aDamageModificationsVariance seed
    |> (\(damage, newSeed) -> getStep6bDefenseDamageModification attack damage target.character.defense target.character.magicalDefense)
    |> (\damage -> case damage of
        Damage dam ->
            dam)

type Attack
    = PlayerPhysicalAttack
    | PlayerPhysicalMultipleAttack
    | PlayerMagicalAttack
    | PlayerMagicalMultipleAttack
    | PlayerHealingAttack
    | MonsterPhysicalAttack
    | MonsterPhysicalMultipleAttack
    | MonsterMagicalAttack
    | MonsterMagicalMultipleAttack
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
    , hitRate: HitRate }

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

type alias Character =
    { vigor : Vigor
    , speed : Speed 
    , stamina : Stamina 
    , magicPower : MagicPower 
    , battlePower : BattlePower 
    , defense : Defense 
    , magicalDefense : MagicalDefense
    , mblock : MBlock 
    , evade : Evade }

type alias PlayableCharacter =
    { character : Character
    , equippedRelics : EquippedRelics }

terra : Character
terra =
    { vigor = Vigor 31
    , speed = Speed 33
    , stamina = Stamina 28
    , magicPower = MagicPower 39
    , battlePower = BattlePower 12
    , defense = Defense 42
    , magicalDefense = MagicalDefense 33
    , mblock = MBlock 7
    , evade = Evade 5 }

playableTerra : PlayableCharacter 
playableTerra =
    { character = terra
    , equippedRelics = { leftHand = Earring, rightHand = Earring } }

locke : Character
locke =
    { vigor = Vigor 37
    , speed = Speed 40
    , stamina = Stamina 31
    , magicPower = MagicPower 28
    , battlePower = BattlePower 14
    , defense = Defense 46
    , magicalDefense = MagicalDefense 23
    , mblock = MBlock 2
    , evade = Evade 15 }

playableLocke : PlayableCharacter
playableLocke =
    { character = locke
    , equippedRelics = { leftHand = HeroRing, rightHand = SneakRing }}

-- 2.1 - Step 1 magical attacks made by characters

getDamageStep1A : Attack -> SpellPower -> MagicPower -> Level -> Damage -> Damage
getDamageStep1A attack (SpellPower spellPower) (MagicPower magicPower) (Level level) (Damage damage) =
    if isMagicalAttack attack then
        Damage (spellPower * 4 + (level * magicPower * spellPower // 32) + damage)
    else
        Damage damage

type Relic
    = NoRelic
    | AtlasArmlet
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
    { leftHand : Relic
    , rightHand : Relic }

-- 2.1 Step 2b
getEquippedWithOneEarring : EquippedRelics -> Bool
getEquippedWithOneEarring { leftHand, rightHand } =
    if leftHand == Earring && rightHand /= Earring then
        True
    else if rightHand == Earring && leftHand /= Earring then
        True
    else
        False

getEquippedWithOneHeroRing : EquippedRelics -> Bool
getEquippedWithOneHeroRing { leftHand, rightHand } =
    if leftHand == HeroRing && rightHand /= HeroRing then
        True
    else if rightHand == HeroRing && leftHand /= HeroRing then
        True
    else
        False

-- 2.1 Step 2c
getEquippedWithTwoEarrings : EquippedRelics -> Bool
getEquippedWithTwoEarrings { leftHand, rightHand } =
    if leftHand == Earring && rightHand == Earring then
        True
    else
        False

getEquippedWithTwoHeroRings : EquippedRelics -> Bool
getEquippedWithTwoHeroRings { leftHand, rightHand } =
    if leftHand == HeroRing && rightHand == HeroRing then
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



-- -- Step 1C

-- type Vigor = Vigor Int

-- clampVigor : Vigor -> Vigor
-- clampVigor (Vigor vigor) =
--     if vigor >= 128 then
--         Vigor 255

--     else
--         Vigor vigor


-- doubleVigor : Vigor -> Vigor
-- doubleVigor (Vigor vigor) =
--     (vigor * 2) |> Vigor |> clampVigor


-- vigorToInt : Vigor -> Int
-- vigorToInt (Vigor vigor) =
--     vigor

-- type AttackPower = AttackPower Int
-- type BattlePower = BattlePower Int

-- getAttackPower : BattlePower -> Vigor -> AttackPower
-- getAttackPower (BattlePower battlePower) vigor =
--     battlePower + (doubleVigor vigor |> vigorToInt)
--     |> AttackPower


-- gauntletIncreasedAttack : AttackPower -> BattlePower -> EquippedWithGauntlet -> AttackPower
-- gauntletIncreasedAttack (AttackPower attackPower) (BattlePower battlePower) (EquippedWithGauntlet equippedWithGauntlet) =
--     if equippedWithGauntlet == True then
--         battlePower * 3 // 4 + attackPower |> AttackPower

--     else
--         AttackPower attackPower

-- type EquippedWithGauntlet = EquippedWithGauntlet Bool

-- getAttackPowerFromEquipment : BattlePower -> Vigor -> EquippedWithGauntlet -> AttackPower
-- getAttackPowerFromEquipment battlePower vigor equippedWithGauntlet =
--     gauntletIncreasedAttack (getAttackPower battlePower vigor) battlePower equippedWithGauntlet


-- getDamageStep1DBasicDamage : BattlePower -> Level -> AttackPower -> Damage
-- getDamageStep1DBasicDamage (BattlePower battlePower) (Level level) (AttackPower attackPower) =
--     battlePower + ((level * level * attackPower) // 256) * 3 // 2 |> Damage

-- type EquippedWithOffering = EquippedWithOffering Bool

-- -- Step 1e
-- getOfferingDecreasedDamage : EquippedWithOffering -> Damage -> Damage
-- getOfferingDecreasedDamage (EquippedWithOffering equippedWithOffering) (Damage damage) =
--     if equippedWithOffering == True then
--         damage // 2 |> Damage

--     else
--         Damage damage

-- type AttackType
--     = StandardFightAttack
--     | MagicAttack

-- type EquippedwithGenjiGlove = EquippedwithGenjiGlove Bool

-- type WeaponCount
--     = NoWeapon
--     | OneWeapon
--     | DualWeapons

-- -- Step 1f
-- getDamageFromGenjiGlove : Attack -> EquippedwithGenjiGlove -> WeaponCount -> Damage -> Damage
-- getDamageFromGenjiGlove attackType (EquippedwithGenjiGlove equippedWithGenjiGlove) weaponCount (Damage damage) =
--     if attackType == PlayerPhysicalAttack && equippedWithGenjiGlove == True && weaponCount /= DualWeapons then
--         Basics.ceiling ( (toFloat damage) * (3 // 4)) |> Damage
--     else
--         Damage damage 


-- addDamage : Int -> Int -> Int
-- addDamage a b =
--     a + b


-- -- getDamageStep1C : Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Int -> Int
-- -- getDamageStep1C vigor battlePower level equippedWithGauntlet equippedWithOffering equippedWithGenjiGlove oneOrZeroWeapons damage =
-- --     let
-- --         attackPower =
-- --             getAttackPowerFromEquipment battlePower vigor equippedWithGauntlet

-- --         getOfferingDecreasedDamageF =
-- --             getOfferingDecreasedDamage equippedWithOffering

-- --         getDamageFromGenjiGloveF =
-- --             getDamageFromGenjiGlove True equippedWithGenjiGlove oneOrZeroWeapons
-- --     in
-- --     getDamageStep1CBasicDamage battlePower level attackPower
-- --         |> getOfferingDecreasedDamageF
-- --         |> getDamageFromGenjiGloveF
-- --         |> addDamage damage


-- getMonsterPhysicalAttackDamage : Attack -> Vigor -> BattlePower -> Level  -> Damage -> Damage
-- getMonsterPhysicalAttackDamage attack (Vigor vigor) (BattlePower battlePower) (Level level) (Damage damage) =
--     case attack of
--         MonsterPhysicalAttack ->
--             level * level * (battlePower * 4 + vigor) // 256 + damage |> Damage

--         _ ->
--             Damage damage


-- -- getDamageStep1 : Attack -> Int -> Int -> Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> Int
-- -- getDamageStep1 attack vigor battlePower spellPower magicPower level equippedWithGauntlet equippedWithOffering equippedWithGenjiGlove oneOrZeroWeapons =
-- --     let
-- --         attackPower =
-- --             getAttackPowerFromEquipment battlePower vigor equippedWithGauntlet

-- --         getDamageStep1AP =
-- --             getDamageStep1A attack spellPower magicPower level 0

-- --         getDamageStep1BP =
-- --             getDamageStep1B attack spellPower magicPower level

-- --         getDamageStep1CP =
-- --             getDamageStep1C vigor battlePower level equippedWithGauntlet equippedWithOffering equippedWithGenjiGlove oneOrZeroWeapons

-- --         getDamageStep1DP =
-- --             getDamageStep1D attack vigor battlePower level attackPower
-- --     in
-- --     getDamageStep1AP
-- --         |> getDamageStep1BP
-- --         |> getDamageStep1CP
-- --         |> getDamageStep1DP



-- -- getRandomMonsterVigor : Seed -> Int
-- -- getRandomMonsterVigor seed =
-- --     Tuple.first (getRandomNumberFromRange 56 63 seed)



-- -- Step 2

-- type EquippedWithAtlasArmlet = EquippedWithAtlasArmlet Bool
type EquippedWithOneHeroRing = EquippedWithOneHeroRing Bool 

-- equippedWithAtlasArmletOrHeroRing : EquippedWithAtlasArmlet -> EquippedWithHeroRing -> Bool
-- equippedWithAtlasArmletOrHeroRing (EquippedWithAtlasArmlet equippedWithAtlasArmlet) (EquippedWithHeroRing equippedWithHeroRing) =
--     if equippedWithAtlasArmlet == True || equippedWithHeroRing == True then
--         True
--     else
--         False

-- getStep2PhysicalAttackRelicBonus : Attack -> EquippedWithAtlasArmlet -> EquippedWithHeroRing -> Damage -> Damage
-- getStep2PhysicalAttackRelicBonus attack atlasArmlet heroRing (Damage damage) =
--     if attack == PlayerPhysicalAttack && equippedWithAtlasArmletOrHeroRing atlasArmlet heroRing then
--         toFloat damage * 5 / 4 |> floor |> Damage
--     else
--         Damage damage

type EquippedWithOneEarring = EquippedWithOneEarring Bool

equippedWithOneEarringOrHeroRing : EquippedWithOneEarring -> EquippedWithOneHeroRing -> Bool
equippedWithOneEarringOrHeroRing (EquippedWithOneEarring equippedWithOneEarring) (EquippedWithOneHeroRing equippedWithOneHeroRing) =
    if equippedWithOneEarring == True || equippedWithOneHeroRing == True then
        True
    else
        False

getStep2MagicalAttackRelicBonus : Attack -> EquippedWithOneEarring -> EquippedWithOneHeroRing -> Damage -> Damage
getStep2MagicalAttackRelicBonus attack equippedWithOneEarring equippedWithHeroRing (Damage damage) =
    if isMagicalAttack attack && equippedWithOneEarringOrHeroRing equippedWithOneEarring equippedWithHeroRing == True then
        damage * 5 // 4 |> Damage
    else
        Damage damage

-- 2.1 Step 2c
type EquippedWithTwoEarrings = EquippedWithTwoEarrings Bool
type EquippedWithTwoHeroRings = EquippedWithTwoHeroRings Bool

equippedWithTwoEaringsOrTwoHeroRings : EquippedWithTwoEarrings -> EquippedWithTwoHeroRings -> Bool
equippedWithTwoEaringsOrTwoHeroRings (EquippedWithTwoEarrings twoEarrings) (EquippedWithTwoHeroRings twoHeroRings) =
    if twoEarrings == True || twoHeroRings == True then
        True
    else
        False

-- 2.1 Step 2c
getStep2MagicalAttackRelicsBonus : Attack -> EquippedWithTwoEarrings -> EquippedWithTwoHeroRings -> Damage -> Damage
getStep2MagicalAttackRelicsBonus attack twoEarrings twoHeroRings (Damage damage) =
    if isMagicalAttack attack && equippedWithTwoEaringsOrTwoHeroRings twoEarrings twoHeroRings == True then
        damage + (damage // 4) + (damage // 4) |> Damage
    else
        Damage damage


-- -- getDamageStep2 : Attack -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Int
-- -- getDamageStep2 attack equippedWithAtlasArmlet equippedWith1HeroRing equippedWith2HeroRings equippedWith1Earring equippedWith2Earrings damage =
-- --     case attack of
-- --         PlayerPhysicalAttack ->
-- --             if equippedWithAtlasArmlet || equippedWith1HeroRing then
-- --                 getStep2SingleDamageBonus damage

-- --             else
-- --                 damage

-- --         PlayerMagicalAttack ->
-- --             if equippedWith1Earring || equippedWith1HeroRing then
-- --                 getStep2SingleDamageBonus damage

-- --             else if equippedWith2Earrings || equippedWith2HeroRings then
-- --                 getStep2DoubleDamageBonus damage

-- --             else
-- --                 damage

-- --         _ ->
-- --             damagetw




-- Step 3

getDamageStep3 : Attack -> Damage -> Damage
getDamageStep3 attack (Damage damage) =
    if attack == PlayerMagicalMultipleAttack then
        damage // 2 |> Damage
    else
        Damage damage



-- -- Step 4

-- type RowPosition
--     = Front
--     | Back

-- getDamageStep4 : Attack -> RowPosition -> Damage -> Damage
-- getDamageStep4 row damage =
--     if row == Back then
--         toFloat damage / 2 |> floor |> Damage

--     else
--         Damage damage



-- -- Step 5

-- type CriticalHitDamageMultiplier = CriticalHitDamageMultiplier Int

-- startingCriticalHitMultiplier : CriticalHitDamageMultiplier
-- startingCriticalHitMultiplier =
--     CriticalHitDamageMultiplier 0

-- -- getCriticalHit : Seed -> Bool
-- -- getCriticalHit seed =
-- --     Tuple.first (getRandomNumberFromRange 1 32 seed) == 32

-- type HasMorphStatus = HasMorphStatus Bool

-- getMorphStatusMultiplier : HasMorphStatus -> CriticalHitDamageMultiplier -> CriticalHitDamageMultiplier
-- getMorphStatusMultiplier (HasMorphStatus hasMorphStatus) (CriticalHitDamageMultiplier multiplier) =
--     if hasMorphStatus == True then
--         CriticalHitDamageMultiplier (multiplier + 2)

--     else
--         CriticalHitDamageMultiplier multiplier

-- type HasBerserkStatus = HasBerserkStatus Bool

-- getBerserkStatusAndPhysicalAttackMultiplier : Attack -> HasBerserkStatus -> CriticalHitDamageMultiplier -> CriticalHitDamageMultiplier
-- getBerserkStatusAndPhysicalAttackMultiplier attack (HasBerserkStatus hasBerserkStatus) (CriticalHitDamageMultiplier multiplier) =
--     if hasBerserkStatus == True && attack == PlayerPhysicalAttack then
--         CriticalHitDamageMultiplier (multiplier + 1)
--     else
--         CriticalHitDamageMultiplier multiplier


-- type CriticalHit = CriticalHit Bool

-- getCriticalHitMultiplier : CriticalHit -> CriticalHitDamageMultiplier -> CriticalHitDamageMultiplier
-- getCriticalHitMultiplier (CriticalHit criticalHit) (CriticalHitDamageMultiplier multiplier) =
--     if criticalHit == True then
--         CriticalHitDamageMultiplier (multiplier + 2)
--     else
--         CriticalHitDamageMultiplier multiplier

-- getStep5Damage : CriticalHitDamageMultiplier -> Damage -> Damage
-- getStep5Damage (CriticalHitDamageMultiplier multiplier) (Damage damage) =
--     (toFloat damage / 2) * (toFloat multiplier) |> floor |> Damage


-- -- getStep5DamageMultiplier : Attack -> Bool -> Bool -> Bool -> number
-- -- getStep5DamageMultiplier attack hasMorphStatus hasBerserkStatus isCriticalHit =
-- --     let
-- --         multiplier =
-- --             0
-- --     in
-- --     getMorphStatusMultiplier hasMorphStatus multiplier
-- --         |> getBerserkStatusAndPhysicalAttackMultiplier attack hasBerserkStatus
-- --         |> getCriticalHitMultiplier isCriticalHit


-- -- getDamageStep5 : Attack -> Bool -> Bool -> Bool -> Int -> Int
-- -- getDamageStep5 attack hasMorphStatus hasBerserkStatus isCriticalHit damage =
-- --     getStep5Damage
-- --         (getStep5DamageMultiplier attack hasMorphStatus hasBerserkStatus isCriticalHit)
-- --         damage



-- Step 6
-- 224 was what I had defaulted to

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

type MagicalDefense = MagicalDefense Int

getMagicalDefenseModification : Damage -> MagicalDefense -> Damage 
getMagicalDefenseModification (Damage damage) (MagicalDefense defense) =
    (damage * (256 - defense) // 256) + 1 |> Damage

getStep6bDefenseDamageModification : Attack -> Damage -> Defense -> MagicalDefense -> Damage
getStep6bDefenseDamageModification attack damage def mblock =
    if isPhysicalAttack attack then
        getPhysicalDefenseModification damage def
    else if isMagicalAttack attack then
        getMagicalDefenseModification damage mblock
    else
        damage 


-- type HasSafeStatus = HasSafeStatus Bool

-- getPhysicalAttackAgainstSafeTarget : Attack -> HasSafeStatus -> Damage -> Damage
-- getPhysicalAttackAgainstSafeTarget attack (HasSafeStatus safeStatus) (Damage damage) =
--     if attack == PlayerPhysicalAttack || attack == MonsterPhysicalAttack && safeStatus == True then
--         ((toFloat damage) * 170 / 256) + 1 |> floor |> Damage
--     else
--         Damage damage

-- type HasShellStatus = HasShellStatus Bool 

-- getMagicalAttackAgainstShellTarget : Attack -> HasShellStatus -> Damage -> Damage
-- getMagicalAttackAgainstShellTarget attack (HasShellStatus shellStatus) (Damage damage) =
--     if attack == PlayerMagicalAttack || attack == MonsterMagicalAttack && shellStatus == True then
--         ((toFloat damage) * 170 / 256) + 1 |> Damage
--     else
--         Damage damage 

-- type TargetDefending = TargetDefending Bool 

-- getStep6DefendingModification : Attack -> TargetDefending -> Damage -> Damage
-- getStep6DefendingModification attack (TargetDefending defending) (Damage damage) =
--     if attack == PlayerPhysicalAttack || attack == MonsterPhysicalAttack && defending == True then
--         toFloat damage / 2 |> floor |> Damage
--     else
--         Damage damage

-- getStep6BackRowModification : Attack -> RowPosition -> Damage -> Damage
-- getStep6BackRowModification attack row (Damage damage) =
--     if attack == PlayerPhysicalAttack || attack == MonsterPhysicalAttack && row == Back then
--         toFloat damage / 2 |> floor |> Damage
--     else
--         Damage damage


-- getStep6MorphModification : Attack -> HasMorphStatus -> Damage -> Damage
-- getStep6MorphModification attack (HasMorphStatus morphed) (Damage damage) =
--     if attack == PlayerMagicalAttack || attack == MonsterMagicalAttack && morphed == True then
--         toFloat damage / 2 |> floor |> Damage
--     else
--         Damage damage

-- type TargetIsSelf = TargetIsSelf Bool
-- type TargetIsCharacter = TargetIsCharacter Bool

-- getStep6HealingAttack : Attack -> TargetIsSelf -> TargetIsCharacter -> Damage -> Damage
-- getStep6HealingAttack attack (TargetIsSelf isSelf) (TargetIsCharacter isCharacter) (Damage damage) =
--     case attack of
--         PlayerHealingAttack ->
--             Damage damage
--         _ ->
--             if isSelf == True && isCharacter == True then
--                 toFloat damage / 2 |> floor |> Damage

--             else
--                 Damage damage


-- -- getStep6Damage :
-- --     Attack
-- --     -> Int
-- --     -> Int
-- --     -> Int
-- --     -> Bool
-- --     -> Bool
-- --     -> Bool
-- --     -> Bool
-- --     -> Bool
-- --     -> Bool
-- --     -> Bool
-- --     -> Int
-- --     -> Int
-- -- getStep6Damage attack variance defense magicalDefense targetHasSafeStatus targetHasShellStatus targetDefending targetIsInBackRow targetHasMorphStatus targetIsSelf targetIsCharacter damage =
-- --     getDamageStep6Basic variance damage
-- --         |> getStep6DefenseModification attack defense magicalDefense
-- --         |> getStep6SafeShellModification attack targetHasSafeStatus targetHasShellStatus
-- --         |> getStep6DefendingModification attack targetDefending
-- --         |> getStep6BackRowModification attack targetIsInBackRow
-- --         |> getStep6MorphModification attack targetHasMorphStatus
-- --         |> getStep6HealingAttack attack targetIsSelf targetIsCharacter



-- -- Step 7

-- type DamageMultiplier = DamageMultiplier Int

-- type HittingTargetsBack = HittingTargetsBack Bool

-- initialDamageMultiplier : DamageMultiplier
-- initialDamageMultiplier =
--     DamageMultiplier 0

-- getStep7DamageMultiplier : Attack -> HittingTargetsBack -> DamageMultiplier -> DamageMultiplier
-- getStep7DamageMultiplier attack (HittingTargetsBack hittingTargetsBack) (DamageMultiplier multiplier) =
--     if attack == PlayerPhysicalAttack || attack == MonsterPhysicalAttack && hittingTargetsBack == True then
--         DamageMultiplier (multiplier + 1)
--     else
--         DamageMultiplier multiplier

-- getStep7Damage : Attack -> HittingTargetsBack -> DamageMultiplier -> Damage -> Damage
-- getStep7Damage attack hittingTargetsBack multiplier (Damage damage) =
--     case getStep7DamageMultiplier attack hittingTargetsBack multiplier of
--         DamageMultiplier multi ->
--             damage + ((damage // 2) * multi) |> Damage


-- -- Step 8

-- type HasPetrifyStatus = HasPetrifyStatus Bool

-- getDamageStep8 : HasPetrifyStatus -> Damage -> Damage
-- getDamageStep8 (HasPetrifyStatus petrified) damage =
--     if petrified == True then
--         Damage 0

--     else
--         damage



-- -- Step 9


-- type ElementEffect
--     = ElementHasBeenNullified
--     | TargetAbsorbsElement
--     | TargetIsImmuneToElement
--     | TargetIsResistantToElement
--     | TargetIsWeakToElement


-- -- TODO/FIXME: This needs to be procedural, but not sure how to do that yet.
-- getDamageStep9 : ElementEffect -> Damage -> Damage
-- getDamageStep9 elementEffect (Damage damage) =
--     case elementEffect of
--         ElementHasBeenNullified ->
--             Damage 0

--         TargetAbsorbsElement ->
--             Damage (damage * -1)

--         TargetIsImmuneToElement ->
--             Damage 0

--         TargetIsResistantToElement ->
--             toFloat damage / 2 |> floor |> Damage

--         TargetIsWeakToElement ->
--             Damage (damage * 2)



-- -- Hit Determinism


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

-- type AttackResult
--     = Hit
--     | HitAndRemoveImageStatus
--     | Miss 
--     | MissAndRemoveImageStatus
--     | Unknown


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


isMagicalAttack : Attack -> Bool
isMagicalAttack attack =
    case attack of
        MonsterMagicalAttack ->
            True

        MonsterMagicalMultipleAttack ->
            True

        PlayerMagicalAttack ->
            True

        PlayerMagicalMultipleAttack ->
            True

        _ ->
            False


-- isSpecialAttack : Attack -> Bool
-- isSpecialAttack attack =
--     isPhysicalAttack attack == False && isMagicalAttack attack == False

-- -- 2.2 Step 1

-- type HasClearStatus = HasClearStatus Bool 

-- attackAgainstClearTarget : Attack -> HasClearStatus -> AttackResult
-- attackAgainstClearTarget attack (HasClearStatus clear) =
--     if isPhysicalAttack attack && clear == True then
--         Miss

--     else if isMagicalAttack attack && clear == True then
--         Hit

--     else
--         Unknown

-- -- 2.2 Step 2

-- type ProtectedFromWound = ProtectedFromWound Bool
-- type AttackMissesDeathProtectedTargets = AttackMissesDeathProtectedTargets Bool

-- anyAttackWoundProtectMissDeath : ProtectedFromWound -> AttackMissesDeathProtectedTargets -> AttackResult
-- anyAttackWoundProtectMissDeath (ProtectedFromWound protectedFromWound) (AttackMissesDeathProtectedTargets attackMissesDeathProtectedTargets) =
--     if protectedFromWound && attackMissesDeathProtectedTargets then
--         Miss

--     else
--         Unknown

-- -- 2.2 Step 3

-- type SpellUnblockable = SpellUnblockable Bool

-- magicalAttackSpellUnblockable : Attack -> SpellUnblockable -> AttackResult
-- magicalAttackSpellUnblockable attack (SpellUnblockable spellUnblockable) =
--     if isMagicalAttack attack && spellUnblockable then
--         Hit

--     else
--         Unknown

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
