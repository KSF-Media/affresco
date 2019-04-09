# Common env variables groupped by their purpose
env_variables = {
  "social_login" => %w[
    PRODUCTION_JANRAIN_LOGIN_CLIENT_ID
    PRODUCTION_JANRAIN_SSO_SERVER
    PRODUCTION_JANRAIN_FLOW_VERSION
    PRODUCTION_JANRAIN_XD_RECEIVER_PATH
    PRODUCTION_GOOGLE_CLIENT_ID
    PRODUCTION_FACEBOOK_APP_ID
  ],
  "persona" => %w[
    PRODUCTION_PERSONA_URL
  ]
}

# A hash of apps with their configuration
apps = {
  "mitt-konto" => {
    "packages" => [ 'MittKonto.Main' ],
    "env_variables" =>
    env_variables["social_login"] +
    env_variables["persona"]
  },
  "prenumerera" => {
    "packages" => [ 'Prenumerera.Main' ],
    "env_variables" =>
    env_variables["social_login"] +
    env_variables["persona"]
  },
  "elections" => {
    "packages" => [ ],
    "env_variables" => %w[]
  }
}

app_name = ARGV.first

abort("Invalid app name: #{app_name}") if !apps.keys.include?(app_name)

if ENV['HEAD'] == 'master'
  apps[app_name]["env_variables"].each do |v|
    abort("Did not find #{v} in the environment variables") if ENV[v].nil?
  end

  File.open("apps/#{app_name}/.env.production", 'a') do |f|
    apps[app_name]["env_variables"].each do |v|
      # Strip 'PRODUCTION_' from the variable name
      env_var_name = v.sub(/^PRODUCTION_/, '')
      f.puts("#{env_var_name}=#{ENV[v]}")
    end
  end

  ENV['NODE_ENV'] = 'production'
else
  ENV['NODE_ENV'] = 'development'
end

def run_command(command)
  puts "Running '#{command}'"
  system(command) or abort("'#{command}' failed.")
end

modules_to_build = apps[app_name]["packages"].map {|x| "'" + x + "'"}

build_commands = [
  "yarn run clean",
  "yarn install --pure-lockfile --cache-folder=.yarn-cache",
  "lerna clean --yes",
  modules_to_build.empty? ? nil : "yarn run --cache-folder=.yarn-cache build-purs #{modules_to_build.join(' ')}",
  "lerna bootstrap",
  "lerna run --cache-folder=.yarn-cache --scope='@affresco/#{app_name}' build"
].compact

build_commands.each { |c| run_command(c) }
