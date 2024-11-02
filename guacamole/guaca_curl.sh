#!/usr/bin/env bash

## Retrieve token bash
token="$(curl -X POST 'http://localhost:8080/guacamole/api/tokens' \
    -d 'username=guacadmin&password=guacadmin' | jq -r '.authToken')"

echo "TK: $token"

## Create connection bash
curl -X POST 'http://localhost:8080/guacamole/api/session/data/mysql/connections' \
    -H "Guacamole-Token: ${token}" \
    -H 'Content-Type: application/json' \
    -d '{
          "name": "Test Connection",
          "protocol": "ssh",
          "parameters": {
              "hostname": "192.168.1.134",
              "port": "22",
              "username": "user",
              "password": "12"
          }
        }'
