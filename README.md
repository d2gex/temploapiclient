[![Documentation](https://img.shields.io/badge/documentation-temploapiclient-orange.svg?colorB=E91E63)](https://d2gex.github.io/temploapiclient/)
# temploapiclient

`temploapiclient` is a lightweight REST client built on [httpeasyrest](https://github.com/d2gex/httpeasyrest)
designed to consume the API made available by the [TEMPLO](https://galtrackingnet.com/en/research/list-projects/projects/templo-new-technologies-for-the-observation-of-the-marine-environment-implementation-of-live-acoustic-telemetry-on-observation-platforms/) project.
Live data is transmitted directly from the ocean every two minutes, either from a coastal network of receptors measuring 
oceanographic variables or from fish equipped with small transmitters for detection. 

Detections are generated by tags surgically implanted in fish, which emit signals as the these pass by any hydrophone in 
the deployed acoustic telemetry network. In addition to tracking detections, these tags measure physical variables such 
as depth, pressure, and temperature.

# High-level system design
Currently, all clients sending data to the servers from the ocean are using the same MQTT THELMA gateway. As shown in
the figure below, the bottom branch of the `CETMAR SETTING UP` section is actually connected to `THELMA MQTT Broker`, 
while the top branch is not being used.

![Templo's high level design](https://github.com/d2gex/temploapiclient/blob/main/inst/images/acoustic_telemetry_system_design.png?raw=true "Templo's high level design")

# Entity Relationship Diagram
The entity-relationship model of the underlying database over which the API has been built is shown below.

![Templo's entity relationship model](https://github.com/d2gex/temploapiclient/blob/main/inst/images/er_templo.png?raw=true "Templo's entity relationship model")

# Installation
This R package can be installed through the devtools as follows:
```r 
  devtools::install_github("https://github.com/d2gex/temploapiclient", dep=TRUE)
```
