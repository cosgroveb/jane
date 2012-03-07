def rebar(args, skip_deps=true)
  args = args.to_s + " skip_deps=true" if skip_deps
  sh "./rebar #{args}"
end

desc "Compile the project"
task :compile do
  rebar :compile
end

desc "Clean the project"
task :clean do
  rebar :clean
end

desc "Clean, get deps and compile"
task :deps do
  rebar "clean get-deps compile", false
end


desc "Generate the release"
task :generate do
  rebar :generate
end

desc "Test the project"
task :test do
  rebar :eunit
end

desc "Run development console"
task :console do
  sh "erl -config test -pa ebin deps/*/ebin -s jane"
end

desc "deploy jane"
task :deploy do
  puts "> Getting dependencies"
  rebar "get-deps"

  puts "> Cleaning"
  rebar :clean

  puts "> Compiling"
  rebar :compile

  puts "> Generating the release"
  rebar :generate

  puts "\n> Done!"
end
