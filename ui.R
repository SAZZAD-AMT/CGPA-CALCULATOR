


library(rsconnect)

# Set your account information
rsconnect::setAccountInfo(name='cgpacalculator', token='839EB9796E816193AA156197A925F2CB', secret='cdKdX5EhAwjxIXC2PN+rAtXfAEqRkaCNfmo7UfhU')

# Specify the directory containing your Shiny app files
app_directory <- "C:/Users/Administrator/Desktop/CGPA/"  # Replace with the actual directory path

# Deploy the Shiny app
rsconnect::deployApp(appDir = app_directory)
