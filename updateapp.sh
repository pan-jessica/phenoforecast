#!/bin/bash
cd /srv/shiny-server/phenoforecast/
OLD_HEAD=$(sudo git rev-parse HEAD)
sudo git pull
NEW_HEAD=$(sudo git rev-parse HEAD)
if [ $OLD_HEAD != $NEW_HEAD ]
then
  /opt/microsoft/ropen/4.0.2/lib64/R/bin/Rscript "/srv/shiny-server/phenoforecast/copyfiles.R"
fi
