--[[
AdiButtonAuras - Display auras on action buttons.
Copyright 2013-2014 Adirelle (adirelle@gmail.com)
All rights reserved.

This file is part of AdiButtonAuras.

AdiButtonAuras is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

AdiButtonAuras is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with AdiButtonAuras.  If not, see <http://www.gnu.org/licenses/>.
--]]

local addonName, addon = ...

local _G = _G
local bit = _G.bit
local error = _G.error
local floor = _G.floor
local format = _G.format
local GetItemInfo = _G.GetItemInfo
local GetSpellInfo = _G.GetSpellInfo
local GetSpellLink = _G.GetSpellLink
local gsub = _G.gsub
local ipairs = _G.ipairs
local math = _G.math
local next = _G.next
local pairs = _G.pairs
local select = _G.select
local setfenv = _G.setfenv
local strjoin = _G.strjoin
local strmatch = _G.strmatch
local tinsert = _G.tinsert
local tonumber = _G.tonumber
local tostring = _G.tostring
local type = _G.type
local UnitClass = _G.UnitClass
local UnitPower = _G.UnitPower
local UnitPowerMax = _G.UnitPowerMax
local unpack = _G.unpack
local wipe = _G.wipe

local getkeys      = addon.getkeys
local ucfirst      = addon.ucfirst
local ConcatLists  = addon.ConcatLists
local FlattenList  = addon.FlattenList
local AsList       = addon.AsList
local AsSet        = addon.AsSet
local MergeSets    = addon.MergeSets
local BuildKey     = addon.BuildKey

local descriptions = addon.descriptions

local LibPlayerSpells = addon.GetLib('LibPlayerSpells-1.0')
local LibSpellbook = addon.GetLib('LibSpellbook-1.0')

local PLAYER_CLASS = select(2, UnitClass("player"))

-- Local debug with dedicated prefix
local function Debug(...) return addon.Debug('|cffffff00Rules:|r', ...) end

------------------------------------------------------------------------------
-- Rule creation
------------------------------------------------------------------------------

local baseDefPrototype = {}
local baseDefMetatable = { __index = baseDefPrototype }

function baseDefPrototype:Initialize()
	if not self:IsValid() then
		error(format("Invalid definition for %s", self.configKey))
	end
	local name = self:GetName()
	for ruleKey, rule in pairs(self.rules) do
		if name and rule.desc then
			descriptions[ruleKey] = ucfirst(gsub(rule.desc or "", "@NAME", name))
		end
	end
end

function baseDefPrototype:GetConfiguration()

	local enabled = false
	for ruleKey in pairs(self.rules) do
		if addon.db.profile.rules[ruleKey] then
			enabled = true
			break
		end
	end

	if not enabled or not self:IsAvailable() then return end

	if not self.conf then
		self.conf = { name = self:GetName(), units = {}, events = {}, handlers = {}, keys = {} }
	end
	wipe(self.conf.units)
	wipe(self.conf.events)
	wipe(self.conf.handlers)
	wipe(self.conf.keys)
	for ruleKey, rule in pairs(self.rules) do
		if addon.db.profile.rules[ruleKey] then
			tinsert(conf.keys, rule.ruleKey)
			MergeSets(conf.units, rule.units)
			MergeSets(conf.events, rule.events)
			MergeSets(conf.handlers, rule.handlers)
		end
	end
	return self.conf
end

local spellDefPrototype = setmetatable({}, baseDefMetatable)
local spellDefMetatable = { __index = spellDefPrototype }

function spellDefPrototype:GetName()
	return (GetSpellInfo(self.id))
end

function spellDefPrototype:IsValid()
	return self:GetName() ~= nil
end

function spellDefPrototype:IsAvailable()
	if not LibSpellbook:IsKnown(self.id) then
		Debug('Unknown spell:', self.key)
		return false
	end
	if not self.providers then return true end
	for _, provider in ipairs(self.providers) do
		if LibSpellbook:IsKnown(provider) then
			return true
		end
	end
	Debug(self.key..', no providers found: ', unpack(self.providers))
	return false
end

local itemDefPrototype = setmetatable({}, baseDefMetatable)
local itemDefMetatable = { __index = itemDefPrototype }

function itemDefPrototype:GetName()
	return (GetItemInfo(self.id))
end

function itemDefPrototype:IsValid()
	return true
end

function itemDefPrototype:IsAvailable()
	return true
end

local function ParseValue(value, callLevel)
	local spellId = tonumber(type(value) == "string" and strmatch(value, "spell:(%d+)") or value)
	if spellId then
		return 'spell:'..spellId, spellId, spellDefMetatable
	end

	local itemId = tonumber(strmatch(tostring(value), "item:(%d+)"))
	if itemId then
		return'item:'..itemId, itemId, itemDefMetatable
	end

	error(format("Invalid spell or item identifier: %s", tostring(value)), callLevel+1)
end

local definitions = {}

local function AddRule(ruleKey, desc, spell, units, events, handlers, providers, callLevel)
	local configKey, id, metatable = ParseValue(spell, (callLevel or 1)+1)
	
	if not definitions[configKey] then
		definitions[configKey] = setmetatable(
			{
				configKey = configKey,
				id = id,
				providers = prodivers,
				rules = {}
			},
			metatable
		)
	end
	
	definitions[configKey][ruleKey] = { units = units, events = events, handlers = handlers, desc = desc }
end

local function CheckRuleArgs(units, events, handlers, providers, callLevel)
	local numUnits, numEvents

	units, numUnits = AsSet(units, "string", callLevel+1)
	if numUnits == 0 then
		error("Empty unit list", callLevel+1)
	end

	events, numEvents = AsSet(events, "string", callLevel+1)
	if numEvents == 0 then
		error("Empty event list", callLevel+1)
	end

	handlers = AsList(handlers, "function", callLevel+1)
	if #handlers == 0 then
		error("Empty handler list", callLevel+1)
	end

	providers = providers and AsList(providers, "number", callLevel+1) or nil

	return units, events, handlers, providers
end

local function Configure(key, desc, spells, units, events, handlers, providers, callLevel)
	callLevel = callLevel or 1
	spells = AsList(spells)
	if #spells == 0 then
		error("Empty spell list", callLevel+1)
	end
	units, events, handlers, providers = CheckRuleArgs(units, events, handlers, providers, callLevel+1)
	for i, spell in ipairs(spells) do
		AddRule(key, desc, spell, units, events, handlers, providers)
	end
end

local function ConfigureDynamic(callback)
	--addon:RegisterRuleBuilder(callback)
end

local rules = setmetatable({}, {
	__index = function(self, key)
		local def = definitions[key]
		local result = def and def:GetConfiguration() or false
		self[key] = result
		return result
	end
})

LibSpellbook.RegisterCallback(rules, 'LibSpellbook_Spell_Added', function(event, id)
	rules['spell:'..id] = nil
end)

LibSpellbook.RegisterCallback(rules, 'LibSpellbook_Spell_Removed', function(event, id)
	rules['spell:'..id] = false
end)

addon.RegisterMessage(definitions, addon.INITIALIZE_RULES, function()
	for key, def in pairs(definitions) do
		def:Initialize()
	end
end)

addon.rules = rules

------------------------------------------------------------------------------
-- Rule description
------------------------------------------------------------------------------

local L = addon.L
local filterDescs = {
	["HELPFUL"] = L['the buff'],
	["HARMFUL"] = L['the debuff'],
	["HELPFUL PLAYER"] = L['your buff'],
	["HARMFUL PLAYER"] = L['your debuff'],
}
local tokenDescs = {
	player = L['yourself'],
	pet    = L['your pet'],
	ally   = L['the targeted ally'],
	enemy  = L['the targeted enemy'],
	group  = L['the group members'],
}
local highlightDescs = {
	flash   = L['flash'],
	good    = L['show the "good" border'],
	bad     = L['show the "bad" border'],
	lighten = L['lighten'],
	darken  = L['darken'],
	hint    = L['suggest'], -- Not really an highlight but who cares ?
}

local function DescribeHighlight(highlight)
	return highlight and highlightDescs[highlight] or L["show duration and/or stack count"]
end

local function DescribeFilter(filter)
	return filter and (filterDescs[filter] or tostring(filter)) or ""
end

local function DescribeAllTokens(token, ...)
	if token ~= nil then
		return tokenDescs[token] or token, DescribeAllTokens(...)
	end
end

local function DescribeAllSpells(id, ...)
	if id ~= nil then
		local name = type(id) == "number" and GetSpellInfo(id) or tostring(id)
		return name, DescribeAllSpells(...)
	end
end

local function BuildDesc(filter, highlight, token, spell)
	local tokens = type(token) == "table" and DescribeAllTokens(unpack(token)) or DescribeAllTokens(token)
	local spells = type(spell) == "table" and DescribeAllSpells(unpack(spell)) or DescribeAllSpells(spell)
	return ucfirst(gsub(format(
		L["%s when %s %s is found on %s."],
		DescribeHighlight(highlight),
		DescribeFilter(filter),
		spells or "",
		tokens or "?"
	), "%s+", " "))
end

local function DescribeLPSSource(category)
	if category then
		local _, interface, rev = LibPlayerSpells:GetVersionInfo(category)
		return format("LPS-%s-%d.%d.%d-%d", category, interface/10000, (interface/100)%100, interface%100, rev)
	end
end

addon.DescribeHighlight = DescribeHighlight
addon.DescribeFilter = DescribeFilter
addon.DescribeAllTokens = DescribeAllTokens
addon.DescribeAllSpells = DescribeAllSpells
addon.BuildDesc = BuildDesc
addon.DescribeLPSSource = DescribeLPSSource

------------------------------------------------------------------------------
-- Handler builders
------------------------------------------------------------------------------

local function BuildAuraHandler_Single(filter, highlight, token, buff, callLevel)
	local GetAura = addon.GetAuraGetter(filter)
	return function(units, model)
		local found, count, expiration = GetAura(units[token], buff)
		if found then
			model.highlight, model.count, model.expiration = highlight, count, expiration
			return true
		end
	end
end

local function BuildAuraHandler_Longest(filter, highlight, token, buffs, callLevel)
	callLevel = callLevel or 1
	local numBuffs
	buffs, numBuffs = AsSet(buffs, "number", callLevel+1)
	if numBuffs == 1 then
		return BuildAuraHandler_Single(filter, highlight, token, next(buffs), callLevel+1)
	end
	local IterateAuras = addon.GetAuraIterator(filter)
	return function(units, model)
		local longest = -1
		for i, id, count, expiration in IterateAuras(units[token]) do
			if buffs[id] and expiration > longest then
				longest = expiration
				if highlight == "flash" or model.highlight ~= "flash" then
					model.highlight = highlight
				end
				model.count, model.expiration = count, expiration
			end
		end
		return longest > -1
	end
end

local function BuildAuraHandler_FirstOf(filter, highlight, token, buffs, callLevel)
	callLevel = callLevel or 1
	local numBuffs
	buffs, numBuffs = AsSet(buffs, "number", callLevel+1)
	if numBuffs == 1 then
		return BuildAuraHandler_Single(filter, highlight, token, next(buffs), callLevel+1)
	end
	local IterateAuras = addon.GetAuraIterator(filter)
	return function(units, model)
		for i, id, count, expiration in IterateAuras(units[token]) do
			if buffs[id] then
				if highlight == "flash" or model.highlight ~= "flash" then
					model.highlight = highlight
				end
				model.count, model.expiration = count, expiration
				return true
			end
		end
	end
end

------------------------------------------------------------------------------
-- High-callLevel helpers
------------------------------------------------------------------------------

local function Auras(filter, highlight, unit, spells)
	local key = BuildKey('Auras', filter, highlight, unit)
	local desc = BuildDesc(filter, highlight, unit, '@NAME')
	for i, spell in ipairs(AsList(spells, "number", 2)) do
		Configure(key, desc, spell, unit,  "UNIT_AURA",  BuildAuraHandler_Single(filter, highlight, unit, spell, 2), 2)
	end
end

local function PassiveModifier(passive, spell, buff, unit, highlight)
	unit = unit or "player"
	highlight = highlight or "good"
	local handler = BuildAuraHandler_Single("HELPFUL PLAYER", highlight, unit, buff, 3)
	local key = BuildKey("PassiveModifier", passive, spell, buff, unit, highlight)
	local desc = BuildDesc("HELPFUL PLAYER", highlight, unit, buff)
	Configure(key, desc, spell, unit, "UNIT_AURA", handler, passive, 3)
end

local function AuraAliases(filter, highlight, unit, spells, buffs)
	buffs = AsList(buffs or spells, "number", 3)
	local key = BuildKey("AuraAliases", filter, highlight, unit, spells, buffs)
	local desc = BuildDesc(filter, highlight, unit, buffs)
	Configure(key, desc, spells, unit, "UNIT_AURA", BuildAuraHandler_FirstOf(filter, highlight, unit, buffs, 3), nil, 3)
end

local function ShowPower(spells, powerType, handler, highlight, desc)
	if type(powerType) ~= "string" then
		error("Invalid power type value, expected string, got "..type(powerType), 3)
	end
	local powerIndex = _G["SPELL_POWER_"..powerType]
	if not powerIndex then
		error("Unknown power "..powerType, 3)
	end
	local key = BuildKey("ShowPower", powerType, highlight)
	local powerLoc = _G[powerType]
	local actualHandler
	if type(handler) == "function" then
		-- User-supplied handler
		actualHandler = function(_, model)
			return handler(UnitPower("player", powerIndex), UnitPowerMax("player", powerIndex), model, highlight)
		end
	elseif type(handler) == "number" then
		-- A number
		local sign = handler < 0 and -1 or 1
		if not highlight then
			highlight = "flash"
		end
		if handler >= -1.0 and handler <= 1.0 then
			-- Consider the handler as a percentage
			actualHandler = function(_, model)
				local current, maxPower = UnitPower("player", powerIndex), UnitPowerMax("player", powerIndex)
				if maxPower ~= 0 and sign * current / maxPower >= handler then
					model.highlight = highlight
				end
			end
			desc = format(L["Show %s and %s when %s."],
				powerLoc,
				highlightDescs[highlight],
				format(
					sign < 0 and L["it is below %s"] or L["it is above %s"],
					floor(100 * sign * handler)..'%'
				)
			)
		else
			-- Consider the handler as a an absolute value
			actualHandler = function(_, model)
				local current, maxPower = UnitPower("player", powerIndex)
				if UnitPowerMax("player", powerIndex) ~= 0 and sign * current >= handler then
					model.highlight = highlight
				end
			end
			desc = format(L["Show %s and %s when %s."],
				powerLoc,
				highlightDescs[highlight],
				format(
					sign < 0 and L["it is below %s"] or L["it is above %s"],
					sign * handler
				)
			)
		end
	elseif not handler then
		-- Provide a simple handler, that shows the current power value and highlights when it reaches the maximum
		actualHandler = function(_, model)
			local current, maxPower = UnitPower("player", powerIndex), UnitPowerMax("player", powerIndex)
			if current > 0 and maxPower > 0 then
				model.count = current
				if highlight and current == maxPower then
					model.highlight = highlight
				end
			end
		end
		if highlight then
			desc = format(L["Show %s and %s when it reaches its maximum."], powerLoc, highlightDescs[highlight])
		else
			desc = format(L["Show %s."], powerLoc)
		end
	else
		error("Invalid handler type, expected function, number or nil, got "..type(handler), 3)
	end
	Configure(key, desc, spells, "player", { "UNIT_POWER", "UNIT_POWER_MAX" }, actualHandler, nil, 3)
end

local function FilterOut(spells, exclude)
	local result = {}
	for _, spell in ipairs(spells) do
		if not exclude[spell] then
			tinsert(result, spell)
		end
	end
	return result
end

local ImportPlayerSpells
do
	local band = bit.band
	local UNIQUE_AURA = LibPlayerSpells.constants.UNIQUE_AURA
	local INVERT_AURA = LibPlayerSpells.constants.INVERT_AURA
	local TARGETING = LibPlayerSpells.masks.TARGETING
	local HARMFUL = LibPlayerSpells.constants.HARMFUL
	local PERSONAL = LibPlayerSpells.constants.PERSONAL
	local PET = LibPlayerSpells.constants.PET
	local IMPORTANT = LibPlayerSpells.constants.IMPORTANT

	function ImportPlayerSpells(filter, ...)
		local exceptions = AsSet({...}, "number", 3)
		local builders = {}
		for buff, flags, provider, modified, _, category in LibPlayerSpells:IterateSpells(filter, "AURA", "RAIDBUFF") do
			local providers = provider ~= buff and FilterOut(AsList(provider, "number"), exceptions)
			local spells = FilterOut(AsList(modified, "number"), exceptions)
			if not exceptions[buff] and #spells > 0 and (not providers or #providers > 0) then
				local filter, highlight, token = "HELPFUL", "good", "ally"
				local targeting = band(flags, TARGETING)
				if targeting == HARMFUL then
					filter, highlight, token = "HARMFUL", "bad", "enemy"
				elseif targeting == PERSONAL then
					token = "player"
				elseif targeting == PET then
					token = "pet"
				end
				if band(flags, INVERT_AURA) ~= 0 then
					filter = (filter == "HARMFUL") and "HELPFUL" or "HARMFUL"
				end
				if band(flags, UNIQUE_AURA) == 0 then
					filter = filter.." PLAYER"
				end
				if band(flags, IMPORTANT) ~= 0 then
					highlight = "flash"
				end
				local key = BuildKey('LibPlayerSpell', provider, modified, filter, highlight, token, buff)
				local desc = BuildDesc(filter, highlight, token, buff).." ["..DescribeLPSSource(category).."]"
				local handler = BuildAuraHandler_Longest(filter, highlight, token, buff, 3)
				Configure(key, desc, spells, token, "UNIT_AURA", handler, provider, 3)
			end
		end
	end
end

------------------------------------------------------------------------------
-- Environment setup
------------------------------------------------------------------------------

-- Wrap an existing function to accept all its arguments in a table
local function WrapTableArgFunc(func)
	return function(args)
		return func(unpack(args))
	end
end

-- Base "globals"
local baseEnv = {
	-- Common functions and constatns
	L            = L,
	Debug        = Debug,
	PLAYER_CLASS = PLAYER_CLASS,

	-- Intended to be used un Lua
	ConfigureDynamic         = ConfigureDynamic,
	BuildAuraHandler_Single  = BuildAuraHandler_Single,
	BuildAuraHandler_Longest = BuildAuraHandler_Longest,
	BuildAuraHandler_FirstOf = BuildAuraHandler_FirstOf,

	-- Description helpers
	BuildDesc         = BuildDesc,
	BuildKey          = BuildKey,
	DescribeHighlight = DescribeHighlight,
	DescribeFilter    = DescribeFilter,
	DescribeAllTokens = DescribeAllTokens,
	DescribeAllSpells = DescribeAllSpells,
	DescribeLPSSource = DescribeLPSSource,

	-- Basic functions
	Configure = WrapTableArgFunc(Configure),
	ShowPower = WrapTableArgFunc(ShowPower),
	PassiveModifier = WrapTableArgFunc(PassiveModifier),
	ImportPlayerSpells = WrapTableArgFunc(ImportPlayerSpells),

	-- High-level functions
	SimpleDebuffs = function(spells)
		Auras("HARMFUL PLAYER", "bad", "enemy", spells)
	end,

	SharedSimpleDebuffs = function(spells)
		Auras("HARMFUL", "bad", "enemy", spells)
	end,

	SimpleBuffs = function(spells)
		Auras("HELPFUL PLAYER", "good", "ally", spells)
	end,

	SharedSimpleBuffs = function(spells)
		Auras("HELPFUL", "good", "ally", spells)
	end,

	LongestDebuffOf = function(spells, buffs)
		local key = BuildKey('LongestDebuffOf', spells, buffs)
		local desc =  BuildDesc("HARMFUL", "bad", "enemy", buffs)
		Configure(key, desc, spells, "enemy", "UNIT_AURA", BuildAuraHandler_Longest("HARMFUL", "bad", "enemy", buffs or spells, 2), nil, 2)
	end,

	SelfBuffs = function(spells)
		Auras("HELPFUL PLAYER", "good", "player", spells)
	end,

	PetBuffs = function(spells)
		Auras("HELPFUL PLAYER", "good", "pet", spells)
	end,

	BuffAliases = function(args)
		AuraAliases("HELPFUL PLAYER", "good", "ally", unpack(args))
	end,

	DebuffAliases = function(args)
		AuraAliases("HARMFUL PLAYER", "bad", "enemy", unpack(args))
	end,

	SelfBuffAliases = function(args)
		AuraAliases("HELPFUL PLAYER", "good", "player", unpack(args))
	end,
}
for name, func in pairs(addon.AuraTools) do
	baseEnv[name] = func
end

local RULES_ENV = addon.BuildSafeEnv(
	baseEnv,
	-- Allowed Libraries
	{
		"LibDispellable-1.0", "LibPlayerSpells-1.0", "DRData-1.0", "LibSpellbook-1.0", "LibItemBuffs-1.0"
	},
	-- Allowed globals
	{
		"bit", "ceil", "floor", "format", "GetComboPoints", "GetEclipseDirection", "GetNumGroupMembers",
		"GetShapeshiftFormID", "GetSpellBonusHealing", "GetSpellInfo", "GetTime", "GetTotemInfo",
		"ipairs", "math", "min", "pairs", "select", "string", "table", "tinsert", "UnitIsPlayer",
		"UnitCanAttack", "UnitCastingInfo", "UnitChannelInfo", "UnitClass","UnitHealth", "print",
		"UnitHealthMax", "UnitPower",  "UnitPowerMax",  "UnitStagger", "UnitIsDeadOrGhost",
	}
)

function addon.Restricted(func)
	return setfenv(func, RULES_ENV)
end
