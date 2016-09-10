Dockerizing Python Microservices
================================

Why Microservices?
------------------

Pros:
* Smaller lesss complex codebase
* Enable independence between codebase/teams
* More flexible scaling schemes

Cons:
* Complex interdependencies
* Spread out codebase

Why Docker?
-----------

Containers vs VMs:
* Lighter in memory and processing than VMs
* Cached/immutable layered file systems

Tooling:
* Quickly spinning up container/environments
* Easily create/share/publish images
* Unified workflow

Docker Compose
--------------

Specify dependencies in a docker compose file:

    docker-compose up

Workflow
--------

* Docker + Compose replaces Vagrant + VM workflow

Building Good Docker Images
---------------------------

* Each step in a Dockerfile creates a new layer in filesystem
    * MInimize number of `RUN` steps
* Try to make layer cacheable
    * Cached layer reused if no checksum change in source
* Use base images for heavily repeated steps
    * `ONBUILD` for dynamic base images?
* `EXPOSE` ports and volume maps

Python and WSGI Apps
--------------------

* Don't run a web server on your container, use an external proxy or container
* Run WSGI apps using a WSGI app server
* Virtualenvs - don't use!

Debugging containers
--------------------

* Run bash on a running service
* Inspect service logs (stdout and stderr)
* Inspect running container setup

Persistance, configs, & processes
---------------------------------

* Volume Maps
    - Volume maps to external host folder
    - Docker data containers

* Configuration
    - Prefer environment variables

* Managing processes
    - Supervisord or runit
    - Consider refactoring service into multiple containers

Testing + Tooling
-----------------

Testing:

* Control over what is in container = Repeatable workflow
* Cloud based CI options with Docker support

Tooling:

* docker-py: python library for creating tooling with docker

Setting up a cloud
* Load Balancing + Network Topology (HAProxy + Nginx)
* Provisioning (Ansible, Puppet, Salt)
* Monitoring (App Health, Behavior, System Resources)
* Logging (Splunk)

Cloud Infrastructure
--------------------

=> Docker Swarm, Kubernetes, OpenStack (Magnum), CoreOS Fleet

12 Factor Apps
