Flask (David Baumgold)
======================

What
----

* JSON data format
* CRUD operations
* REST semantics
* Flexible code (prototype)

Day 1
-----

Static data with basic flask parameterized endpoints

Day 2
-----

Linking SQL Alchemy

* SQLAlchemy ORM to link to real world data instead of fake

Scripts to bootstrap our DB and create it, can be done with flask/click
integration.

Day 3
-----

Flask Marshmallow

* Transformer between DB resources and external API resources
* Prevents model from needing to know how to transform the db models into json

Flask error handlers for errors

Benefit:

* Data flexbility, modifying the data models just works!
* Automatic, complete error messages (data validation)

Day 4
-----

Flask Login

* Standard way is to provide a authorized header

Day 5
-----

Flask-APISpec (Documentation)
Flask-Limiter (Rate limiting the API)
Pagination-FlaskSQLAlchemy (Pagination for large requests)

SQLAlchemy natively supports multiple databases <---- RESEARCH?
