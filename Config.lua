--[[
AdiButtonAuras - Display auras on action buttons.
Copyright 2013 Adirelle (adirelle@gmail.com)
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
local CreateFrame = _G.CreateFrame
local format = _G.format
local GameTooltip = _G.GameTooltip
local GameTooltip_SetDefaultAnchor = _G.GameTooltip_SetDefaultAnchor
local GetAddOnMetadata = _G.GetAddOnMetadata
local GetCVarBool = _G.GetCVarBool
local GetItemInfo = _G.GetItemInfo
local GetSpellInfo = _G.GetSpellInfo
local InterfaceOptionsFrame_OpenToCategory = _G.InterfaceOptionsFrame_OpenToCategory
local IsAddOnLoaded = _G.IsAddOnLoaded
local IsShiftKeyDown = _G.IsShiftKeyDown
local pairs = _G.pairs
local RAID_CLASS_COLORS = _G.RAID_CLASS_COLORS
local setmetatable = _G.setmetatable
local strjoin = _G.strjoin
local strmatch = _G.strmatch
local tinsert = _G.tinsert
local tonumber = _G.tonumber
local tostring = _G.tostring
local tostringall = _G.tostringall
local UIParent = _G.UIParent
local UISpecialFrames = _G.UISpecialFrames
local UNKNOWN = _G.UNKNOWN
local unpack = _G.unpack
local wipe = _G.wipe

local L = addon.L

local AceConfigRegistry = addon.GetLib('AceConfigRegistry-3.0')

local configOverlays
local selectedKey, selectedName

------------------------------------------------------------------------------
-- Button overlays for selection
------------------------------------------------------------------------------

local configParent = CreateFrame("Frame", addonName.."ConfigOverlay", UIParent)
configParent:Hide()

function configParent:Update()
	AceConfigRegistry:NotifyChange(addonName)
end

configParent:SetScript('OnShow', function(self)
	for _, overlay in addon:IterateOverlays() do
		configOverlays[overlay]:SetShown(overlay:IsVisible())
	end
	self:Update()
end)
configParent:SetScript('OnHide', configParent.Update)
configParent:SetScript('OnEvent', configParent.Hide)
configParent:RegisterEvent('PLAYER_REGEN_DISABLED')

tinsert(UISpecialFrames, configParent:GetName())

-- Overlays

local overlayPrototype = setmetatable({	Debug = addon.Debug}, { __index = CreateFrame("Button") })
local overlayMeta = { __index = overlayPrototype }

configOverlays = setmetatable({}, { __index = function(t, overlay)
	local conf = setmetatable(CreateFrame("Button", overlay:GetName().."Config", configParent), overlayMeta)
	conf:Initialize(overlay)
	t[overlay] = conf
	return conf
end })

local backdrop = {
	bgFile = [[Interface\Tooltips\UI-Tooltip-Background]], tile = true, tileSize = 16,
	insets = { left = 0, right = 0, top = 0, bottom = 0 }
}

function overlayPrototype:Initialize(overlay)
	self:Hide()

	self:SetFrameStrata("HIGH")

	self:SetBackdrop(backdrop)
	self:SetBackdropBorderColor(0,0,0,0)

	self:SetHighlightTexture([[Interface\Buttons\ButtonHilight-Square]], "ADD")

	self.overlay = overlay
	self:SetAllPoints(overlay)
	self:RegisterForClicks('LeftButtonUp')

	self:SetScript('OnShow', self.Update)
	self:SetScript('OnClick', self.OnClick)
	self:SetScript('OnEnter', self.OnEnter)
	self:SetScript('OnLeave', self.OnLeave)

	overlay:HookScript('OnShow', function() self:Show() end)
	overlay:HookScript('OnHide', function() self:Hide() end)

	addon.RegisterMessage(self, addon.CONFIG_CHANGED, "Update")
end

function overlayPrototype:Update()
	self.conf, self.enabled, self.key, self.type, self.id = addon:GetActionConfiguration(self.overlay.spellId)
	if self.type == "spell" then
		self.name = GetSpellInfo(self.id)
	elseif self.type == "item" then
		self.name = GetItemInfo(self.id)
	end
	if self.conf then
		self:Enable()
		if self.enabled then
			self:SetBackdropColor(0, 1, 0, 0.8)
		else
			self:SetBackdropColor(0, 0, 1, 0.8)
		end
		if GameTooltip:GetOwner() == self then
			self:OnEnter()
		end
	else
		self:Disable()
		self:SetBackdropColor(0, 0, 0, 0.8)
		self:OnLeave()
	end
end

function overlayPrototype:OnClick()
	if IsShiftKeyDown() then
		addon.db.profile.enabled[self.key] = not addon.db.profile.enabled[self.key]
		addon.SendMessage(self, addon.CONFIG_CHANGED)
	else
		selectedKey, selectedName = self.key, self.name
	end
	AceConfigRegistry:NotifyChange(addonName)
end

function overlayPrototype:OnEnter()
	GameTooltip_SetDefaultAnchor(GameTooltip, self)
	GameTooltip:AddDoubleLine(self.name, self.type)
	if self.conf then
		if self.enabled then
			GameTooltip:AddDoubleLine(L['Status'], L['Enabled'], nil, nil, nil, 0, 1, 0)
		else
			GameTooltip:AddDoubleLine(L['Status'], L['Disabled'], nil, nil, nil, 0, 0, 1)
		end
		--@debug@
		local title = "Units"
		for unit in pairs(self.conf.units) do
			GameTooltip:AddDoubleLine(title, unit, nil, nil, nil, 1, 1, 1)
			title = " "
		end
		title = "Events"
		for event in pairs(self.conf.events) do
			GameTooltip:AddDoubleLine(title, event, nil, nil, nil, 1, 1, 1)
			title = " "
		end
		GameTooltip:AddDoubleLine(L['Handlers'], #(self.conf.handlers), nil, nil, nil, 1, 1, 1)
		--@end-debug@
		GameTooltip:AddLine(L['Shift+click to toggle.'])
	else
		GameTooltip:AddDoubleLine(L['Status'], UNKNOWN, nil, nil, nil, 0.5, 0.5, 0.5)
	end
	GameTooltip:Show()
end

function overlayPrototype:OnLeave()
	if GameTooltip:GetOwner() == self then
		GameTooltip:Hide()
	end
end
------------------------------------------------------------------------------
-- Version display
------------------------------------------------------------------------------

local function ColorClass(c, ...)
	if c then
		return "|c"..RAID_CLASS_COLORS[c].colorStr..c.."|r", ColorClass(...)
	end
end

local function IdToLink(idstr, ...)
	if not idstr then return end
	local id = tonumber(strmatch(idstr, "^spell:(%d+)$"))
	if id then
		local name, _, icon = GetSpellInfo(id)
		return format("|T%s:0|t %s", icon, name), IdToLink(...)
	else
		return IdToLink(...)
	end
end

local GetVersionInfo
do
	local t = {}
	local p = function(...) tinsert(t, strjoin(" ", tostringall(...))) end
	function GetVersionInfo()
		wipe(t)

		p("\nVersion", "|cffffffff"..tostring(GetAddOnMetadata(addonName, "Version")).."|r")

		p("\nLibraries:")
		for major, minor in pairs(addon.libraries) do
			if minor then
				p("- "..major..": |cffffffff"..tostring(minor).."|r")
			else
				p("- "..major..": |cffff0000NOT FOUND|r")
			end
		end

		local bugGrabber
		if addon.BugGrabber then
			bugGrabber = 'Embedded BugGrabber'
			p("\nError grabber:", "|cffffffff", bugGrabber, "|r")
		elseif IsAddOnLoaded("!BugGrabber") or _G.BugGrabber then
			bugGrabber = "BugGrabber"
		elseif IsAddOnLoaded("!Swatter") or _G.Swatter then
			bugGrabber = "Swatter"
		elseif IsAddOnLoaded("!ImprovedErrorFrame") then
			bugGrabber = "ImprovedErrorFrame"
		elseif GetCVarBool('scriptErrors') then
			bugGrabber = "Blizzard Lua display"
		end
		p("\nError handler:", bugGrabber and ("|cffffffff"..bugGrabber.."|r") or "|cffff0000NONE|r")

		p("\nConfigured spells (spells that are both in your spellbook and", addonName, "rules:")

		p("|cffffffff", strjoin(", ", IdToLink(addon.getkeys(addon.spells))), "|r")

		return table.concat(t, "\n")
	end
end

------------------------------------------------------------------------------
-- Options
------------------------------------------------------------------------------

local options
local function GetOptions()
	if options then return options end

	local profiles = addon.GetLib('AceDBOptions-3.0'):GetOptionsTable(addon.db)
	profiles.order = -10
	profiles.disabled = false

	local tmpRuleList = {}

	options = {
		--@debug@
		name = addonName..' DEV',
		--@end-debug@
		--[===[@non-debug@
		name = addonName..' @project-version@',
		--@end-non-debug@]===]
		type = 'group',
		get = 'Get',
		set =' Set',
		childGroups = 'tab',
		args = {
			global = {
				name = L['Global'],
				type = 'group',
				order = 10,
				get = function(info)
					return addon.db.profile[info[#info]]
				end,
				set = function(info, value)
					addon.db.profile[info[#info]] = value
					addon:SendMessage(addon.CONFIG_CHANGED)
				end,
				args = {
					notInCooldown = {
						name = L['No flash in cooldown'],
						desc = format("%s\n|cffff0000%s|r",
							L['Do not display the flashing animation on actions in cooldown.'],
							L['THIS DOES NOT AFFECT BLIZZARD ANIMATIONS.']
						),
						type = 'toggle',
						order = 10,
					},
					countdownThresholds = {
						name = L["Countdown Thresholds"],
						type = "group",
						inline = true,
						order = -2,
						args = {
							maxCountdown = {
								name = L['Maximum duration to show'],
								desc = L['Durations above this threshold are hidden.'],
								type = 'range',
								width = 'full',
								order = 10,
								min = 0,
								max = 600,
								step = 1,
							},
							minMinutes = {
								name = L['Minimum duration for "2m" format'],
								type = 'range',
								width = 'full',
								order = 20,
								min = 60,
								max = 600,
								step = 1,
							},
							minMinuteSecs = {
								name = L['Minimum duration for "4:58" format'],
								type = 'range',
								width = 'full',
								order = 30,
								min = 60,
								max = 600,
								step = 1,
							},
							maxTenth = {
								name = L['Maximum duration for "2.7" format'],
								type = 'range',
								width = 'full',
								order = 40,
								min = 0,
								max = 60,
								step = 0.5,
							},
						}
					},
					colors = {
						name = "Colors",
						type = "group",
						inline = true,
						order = -1,
						get = function(info)
							return unpack(addon.db.profile.colors[info[#info]], 1, 4)
						end,
						set = function(info, ...)
							local c = addon.db.profile.colors[info[#info]]
							c[1], c[2], c[3], c[4] = ...
							addon:SendMessage(addon.CONFIG_CHANGED)
						end,
						args = {
							good = {
								name = L['"Good" border'],
								desc = L['The color used for good things, usually buffs.'],
								type = 'color',
								hasAlpha = true,
							},
							bad = {
								name = L['"Bad" border'],
								desc = L['The color used for bad things, usually debuffs.'],
								type = 'color',
								hasAlpha = true,
							},
						},
					}
				},
			},
			spells = {
				name = L['Spells & items'],
				desc = L['Configure spells and items individually.'],
				type = 'group',
				order = 20,
				disabled = function(info) return info[#info] ~= "spells" and not selectedKey end,
				args = {
					select = {
						name = function()
							return configParent:IsShown() and L["Hide button highlights"] or L["Show button highlights"]
						end,
						desc = L['Click to show or hide overlay over action buttons.'],
						order = 1,
						type = 'execute',
						width = "double",
						disabled = false,
						func = function()
							configParent:SetShown(not configParent:IsShown())
						end,
					},
					_name = {
						name = function() return selectedName or L["Please select a spell or an item..."] end,
						type = 'header',
						order = 10,
					},
					enabled = {
						name = L['Enabled'],
						desc = L['Uncheck to ignore this spell/item.'],
						order = 20,
						type = 'toggle',
						get = function()
							return addon.db.profile.enabled[selectedKey]
						end,
						set = function(_, flag)
							addon.db.profile.enabled[selectedKey] = flag
							addon:SendMessage(addon.CONFIG_CHANGED)
						end
					},
					inverted = {
						name = L['Inverted'],
						desc = L['Check to show a border when the buff is missing.'],
						order = 30,
						type = 'toggle',
						get = function()
							return addon.db.profile.inverted[selectedKey]
						end,
						set = function(_, flag)
							addon.db.profile.inverted[selectedKey] = flag
							addon:SendMessage(addon.CONFIG_CHANGED)
						end
					},
					rules = {
						name = L['Rules'],
						desc = L['Select which rules should by applied to the button.'],
						order = 40,
						width = 'full',
						type = 'multiselect',
						get = function()
							return true
						end,
						set = function(_, flag)
						end,
						values = function()
							wipe(tmpRuleList)
							local conf = selectedKey and addon.spells[selectedKey]
							if conf then
								for i, key in ipairs(conf.keys) do
									tmpRuleList[i] = addon.ruleDescs[key]
								end
							end
							return tmpRuleList
						end,
					},
				},
			},
			debug = {
				name = 'Debug information',
				type = 'group',
				order = -1,
				args = {
					_text = {
						name = GetVersionInfo,
						type = "description",
						width = 'full',
						fontSize = 'medium',
					},
				},
			},
			profiles = profiles,
		},
	}

	return options
end

------------------------------------------------------------------------------
-- Setup
------------------------------------------------------------------------------

addon.GetLib('AceConfig-3.0'):RegisterOptionsTable(addonName, GetOptions)
local blizPanel = addon.GetLib('AceConfigDialog-3.0'):AddToBlizOptions(addonName, addonName)

function addon:OpenOptions()
	InterfaceOptionsFrame_OpenToCategory(blizPanel)
end

-- Add a macro command to open it
_G.SlashCmdList["ADIBUTTONAURAS"] = function()
	InterfaceOptionsFrame_OpenToCategory(addonName)
end
_G.SLASH_ADIBUTTONAURAS1 = "/adibuttonauras"
_G.SLASH_ADIBUTTONAURAS2 = "/aba"
