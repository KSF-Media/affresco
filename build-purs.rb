#!/usr/bin/env ruby

PursModule = Struct.new(:name, :path)

purs_modules = [
  PursModule.new('KSF.Profile.Component', 'packages/ksf-profile'),
  PursModule.new('KSF.Footer.Component', 'packages/ksf-footer'),
  PursModule.new('KSF.Button.Component', 'packages/ksf-button'),
  PursModule.new('KSF.InputField.Component', 'packages/ksf-input-field'),
  PursModule.new('KSF.Login.Component', 'packages/ksf-login'),
  PursModule.new('KSF.Navbar.Component', 'packages/ksf-navbar'),
  PursModule.new('KSF.DescriptionList.Component', 'packages/ksf-description-list'),
  PursModule.new('KSF.Subscription.Component', 'packages/ksf-subscription'),
  PursModule.new('KSF.Alert.Component', 'packages/ksf-alert'),
  PursModule.new('MittKonto.Main', 'apps/mitt-konto'),
  PursModule.new('SubscribePaper.Main', 'apps/subscribe-paper'),
]

# Name of the modules can be given as an argument as well:
# ./build-purs.rb KSF.Profile.Component KSF.Login.Component
purs_modules_given =
  if ARGV.empty?
    purs_modules
  else
    purs_modules.select { |m| ARGV.include? m.name }
  end

purs_modules_given.each do |mod|
  %x[purp make-module -m #{mod.name} -t #{mod.path}/index.js]
  puts "Built #{mod.name}"
end
