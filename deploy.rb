require 'open3'
require 'json'
require 'yaml'

def run_command(command)
  puts "Running `#{command}`"
  result = ""

  # see: http://stackoverflow.com/a/1162850/83386
  Open3.popen3(command) { |input ,out, err, wait_thread|
    # Store stdout and print it
    Thread.new do
      while line=out.gets do
        puts(line)
        result += line + "\n"
      end
    end
    # But only print stderr
    Thread.new do
      while line=err.gets do
        puts(line)
      end
    end
    # Then wait for da command to be done
    wait_thread.join
    if wait_thread.value.exitstatus != 0
      abort("Command '#{command}' failed")
    end
  }
  return result
end

# A hash of apps with their configuration
# We read that from the deploy info that we use to generate the CI jobs
apps_json = ""
apps_servers_json = ""
begin
  apps_json = run_command("nix-shell ci/dhall.nix --run 'dhall-to-json <<< \"./ci/apps.dhall\"'")
  apps_servers_json = run_command("nix-shell ci/dhall.nix --run 'dhall-to-json <<< \"./ci/app-servers.dhall\"'")
rescue Exception => e
  # FIXME: this is here because Netlify doesn't have nix-shell.
  # This is terrible and should be removed ASAP. Really.
  apps_json = file = File.read('./temp-apps-deprecate-me-asap.json')
end

apps_list = JSON.parse(apps_json)
app_servers_list = JSON.parse(apps_servers_json)
apps = apps_list.map{ |x| [x["deployDir"], x] }.to_h
apps.merge!(app_servers_list.map{ |x| [x["deployDir"], x] }.to_h)

app_name = ARGV.first
maintenance = ENV['MAINTENANCE_MODE']

abort("Invalid app name: #{app_name}") if !apps.keys.include?(app_name)

app = apps[app_name]
app["path"] = "./apps/#{app['deployDir']}"

puts "Branch: #{ENV['GITHUB_REF']}"
puts "Workflow: #{ENV['GITHUB_WORKFLOW']}"

def setup_env(app)
  # Common env variables
  env_variables = %w[
    PRODUCTION_JANRAIN_LOGIN_CLIENT_ID
    PRODUCTION_JANRAIN_SSO_SERVER
    PRODUCTION_JANRAIN_FLOW_VERSION
    PRODUCTION_JANRAIN_XD_RECEIVER_PATH
    PRODUCTION_GOOGLE_CLIENT_ID
    PRODUCTION_FACEBOOK_APP_ID
    PRODUCTION_BOTTEGA_URL
    PRODUCTION_PERSONA_URL
    PRODUCTION_DUELLEN_URL
    PRODUCTION_LETTERA_URL
  ]

  if (ENV['HEAD'] == 'master' or ENV['GITHUB_REF'] == 'refs/heads/master' or ENV['GITHUB_WORKFLOW'] == 'production')
    app_vars = env_variables + app['env'].keys
    app_vars.each do |v|
      abort("Did not find #{v} in the environment variables") if ENV[v].nil?
    end
    # For deploying things to gcloud app engine
    if app.key?('runtime')
      puts "Generating production app.yaml"
      app_yaml                  = {}
      app_yaml['runtime']       = app['runtime']
      app_yaml['service']       = app['id']
      app_yaml['entrypoint']    = app['entrypoint']
      app_yaml['env_variables'] = {}
      app_vars.each do |v|
        env_var_name = v.sub(/^PRODUCTION_/, '')
        app_yaml['env_variables']["#{env_var_name}"] = ENV[v]
      end
      File.open("#{app['path']}/app.yaml", 'w') do |f|
        f.puts(YAML.dump(app_yaml))
      end
    else
      puts "Generating .env.production"
      File.open("#{app['path']}/.env.production", 'a') do |f|
        app_vars.each do |v|
          # Strip 'PRODUCTION_' from the variable name
          env_var_name = v.sub(/^PRODUCTION_/, '')
          f.puts("#{env_var_name}=#{ENV[v]}")
        end
      end
    end
    ENV['NODE_ENV'] = 'production'
  else
    ENV['NODE_ENV'] = 'development'
    if app.key?('runtime')
      run_command("cp #{app['path']}/app.dev.yaml #{app['path']}/app.yaml")
    end
  end
end


build_commands = [
  "yarn install --pure-lockfile --cache-folder=.yarn-cache",
  "yarn --cwd '#{app['path']}/' run build"
]

def deploy_maintenance_page(app_path)
  run_command("mkdir -p #{app_path}/dist && cp ./static/maintenance.html #{app_path}/dist/index.html")
end

if maintenance == 'true'
  puts 'Deploying maintenance page'
  deploy_maintenance_page(app['path'])
elsif app_name == 'scripts'
  Dir.glob("scripts/**/*.js").each do |f|
    `./node_modules/.bin/uglifyjs #{f} -o #{f.gsub(/js\z/, "min.js")}`
  end
  run_command("mkdir -p #{app['path']} && cp -R scripts #{app['path']}/dist")
else
  setup_env(app)
  build_commands.each { |c| run_command(c) }
end
