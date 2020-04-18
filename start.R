library(shiny)

source('durak/ui.R')
source('durak/server.R')

options(app.name = 'Durak')
options(app.description = 'A Russian card game')
options(app.tags = 'Russia,cards,Russian,strategy,game,fun')
options(app.author = 'Jordan Sicherman (jordan.sicherman@msl.ubc.ca)')

runApp('durak', port = 18232, launch.browser = F)

# ssh -L 12345:localhost:18232 jsicherman@nelson -p 22000

# Public tunneling can be achieved by ssh -L 0.0.0.0:12345:localhost:18232 jsicherman@nelson -p 22000
# and then connecting at the public IP (output by the server)