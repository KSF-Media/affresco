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
  },
  "vetrina-staging" => {
    "env_variables" => %w[]
  },
  "scripts" => {
    "env_variables" => %w[]
  }
}

app_name = ARGV.first
maintenance = ARGV[1]

abort("Invalid app name: #{app_name}") if !apps.keys.include?(app_name)

puts "Branch: #{ENV['GITHUB_REF']}"

if (ENV['HEAD'] == 'master' or ENV['GITHUB_REF'] == 'refs/heads/master')
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
  Open3.popen2e(command) { |input ,out_and_err, wait_thread|
    while line=out_and_err.gets do
      puts(line)
    end
    if wait_thread.value.exitstatus != 0
      abort("Command '#{command}' failed")
    end
  }
end

build_commands = [
  "yarn install --pure-lockfile --cache-folder=.yarn-cache",
  "yarn --cwd './apps/#{app_name}/' run build"
]

def deploy_maintenance_page(app_name)
  run_command("mkdir -p ./apps/#{app_name}/dist && cp ./static/maintenance.html ./apps/#{app_name}/dist/index.html")
end

if maintenance == '--maintenance'
  puts 'Deploying maintenance page'
  deploy_maintenance_page(app_name)
elsif app_name == 'scripts'
  Dir.glob("scripts/**/*.js").each { |f|
    `./node_modules/.bin/uglifyjs #{f} -o #{f.gsub(/js\z/, "min.js")}` 
  }
  run_command("mkdir -p ./apps/#{app_name} && cp -R scripts ./apps/#{app_name}/dist")
else
  build_commands.each { |c| run_command(c) }
end
