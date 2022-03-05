# README #
### TOCOMPILE ###
* spago install
## Open two terminals (or) command prompts. In Terminal 1, run below command
* spago build -w
## In Terminal 2, run below command
* spago bundle-app --to dist/app.js --watch
## Open the below directory on File Explorer (Windows)/ Finder (Mac)
$ project_dir/dist/index.html to see the cube

Dev Workflow: Make changes to files in src and it will automatically compile. Reload index.html in the browser.