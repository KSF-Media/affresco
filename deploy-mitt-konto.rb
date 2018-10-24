env_variables = %w[
  PRODUCTION_JANRAIN_LOGIN_CLIENT_ID
  PRODUCTION_JANRAIN_SSO_SERVER
  PRODUCTION_JANRAIN_FLOW_VERSION
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

%x[npm install yarn && yarn install && yarn build-purs && asdasd && lerna bootstrap && lerna run build]

abort("Mitt konto build failed.") if !$?.success?
