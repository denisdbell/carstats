export DATABASE=<Add your database name>
export DBHOST=<Add your database host>
export DBUSER=<Add your database user>
export DBPASSWORD<Add your database password>

#Run the app
uvicorn api:app --host=0.0.0.0 --port=8000 