require 'open3'

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
  ],
  "duellen" => %w[
    PRODUCTION_DUELLEN_URL
  ],
  "sentry" => %w[
    PRODUCTION_SENTRY_DSN
  ]
}

# A hash of apps with their configuration
apps = {
  "mitt-konto" => {
    "env_variables" =>
    env_variables["social_login"] +
    env_variables["persona"] +
    env_variables["sentry"]
  },
  "prenumerera" => {
    "env_variables" =>
    env_variables["social_login"] +
    env_variables["persona"]
  },
  "elections" => {
    "env_variables" => %w[]
  },
  "duellen" => {
    "env_variables" => env_variables["duellen"]
  },
  "app-article" => {
    "env_variables" => env_variables["social_login"] + env_variables["persona"] + ["PRODUCTION_LETTERA_URL"]
  }
}

app_name = ARGV.first
maintenance = ARGV[1]

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
  stdout, stderr, status = Open3.capture3(command)
  if status.exitstatus != 0
    abort("'#{command}' failed: #{stderr}")
  end
end

build_commands = [
  "yarn run clean",
  "yarn install --pure-lockfile --cache-folder=.yarn-cache",
  "yarn --cwd './apps/#{app_name}/' run build"
]

def deploy_maintenance_page(app_name)
  run_command("mkdir -p ./apps/#{app_name}/dist && cp ./static/maintenance.html ./apps/#{app_name}/dist/index.html")
end

if maintenance == '--maintenance'
  puts 'Depolying maintenance page'
  deploy_maintenance_page(app_name)
else
  build_commands.each { |c| run_command(c) }
end
