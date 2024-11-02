# Setup the Guacamole containers
Steps to setup the containers needed to deploy Guacamole server + Guacamole client
- Access in http://localhost:8080/guacamole

## Steps to set everything up for the first time
1. First, we need to generate the initdb.sql file
``` bash
podman run --rm docker.io/guacamole/guacamole /opt/guacamole/bin/initdb.sh --mysql > initdb.sql

# Second run
podman run --rm -e MYSQL_ROOT_PASSWORD=root_password -e MYSQL_DATABASE=guacamole_db -e MYSQL_USER=guacamole_user -e MYSQL_PASSWORD=guacamole_password docker.io/guacamole/guacamole /opt/guacamole/bin/initdb.sh --mysql > initdb.sql
```

2. Once created, we can start the mysql container
``` bash
podman-compose up -d mysql
```

3. Then we need to apply the "host_db" to the "guacamole_db" of the container
``` bash
podman exec -i guacamole-db mysql -u root -proot_password guacamole_db < initdb.sql
```

4. Finally, we can start all the containers
``` bash
podman-compose up -d
```

## Another important commands that WILL be useful
``` bash
# Check the status of the containers, even the ones stopped
podman ps -a

# Remove stopped containers
podman rm container_id # Works with container_name too

# Check the logs of a container
podman logs container_name 

# Remove existing pods by name
podman pod rm pod_name

# Remove any stopped containers, images, or unused pods named "pod_name"
podman pod rm pod_name -f
podman container prune -f
```

## Oh you thought that was all? Xd
Now for some reason Guacadmin has NOT the permission needed to manage connections, so we need to give that user the needed permissions.

We can do it in two ways:
### Giving it the CRUD permissions
I have no time for this lol

### Admin = 1
First we access the DB in the container with the following command
``` bash
podman exec -it guacamole-db mysql -u root -proot_password guacamole_db
```
We should be greated with a precious 'mysql>' prompt

Then, we update the user 'guacadmin' to make him admin
``` mysql
UPDATE guacamole_user SET admin = 1 
WHERE user_id = (SELECT user_id FROM guacamole_user
    WHERE entity_id = (SELECT entity_id FROM guacamole_entity 
        WHERE name = 'guacadmin'));
```
