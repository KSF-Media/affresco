env_variables = %w[
  PRODUCTION_JANRAIN_LOGIN_CLIENT_ID
  PRODUCTION_JANRAIN_SSO_SERVER
  PRODUCTION_JANRAIN_FLOW_VERSION
  PRODUCTION_JANRAIN_XD_RECEIVER_PATH
  PRODUCTION_PERSONA_URL
  PRODUCTION_GOOGLE_CLIENT_ID
  PRODUCTION_FACEBOOK_APP_ID
]

if ENV['HEAD'] == 'master'
  env_variables.each do |v|
    abort("Did not find #{v} in the environment variables") if ENV[v].nil?
  end

  File.open('apps/mitt-konto/.env.production', 'a') do |f|
    env_variables.each do |v|
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

build_commands = [
  'npm run clean',
  'npm install yarn',
  'yarn install --pure-lockfile --cache-folder=.yarn-cache',
  'lerna clean --yes',
  'yarn run --cache-folder=.yarn-cache build-purs',
  'lerna bootstrap',
  'lerna run --cache-folder=.yarn-cache build'
]

build_commands.each { |c| run_command(c) }
