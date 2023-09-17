# schedule

An important practical application at FCUP concerns the preparation of weekly timetables for students, professors and rooms. There is an application being used at DCC for this, but the interface is rather poor, making timetable preparation a painful task.  The aim of this project is to provide a well designed graphical users interface for timetabling, with the specific application at FCUP in view.  Usability should be enlarged for all the departments, with swift resource sharing.  Ideally, this project will deliver a complete prototype.

This repository contains only the frontend of the application, the backend can be found __[here](https://github.com/luismdsleite/schedule-backend)__.

![screenshot](./images/screenshot.png)

## Dependencies

[![elm](https://img.shields.io/badge/Elm-0.19.1-brightgreen.svg?logo=elm)](https://elm-lang.org/)

[![elm-land](https://img.shields.io/badge/elm--land-0.19.2-brightgreen.svg)](https://github.com/elm-land)

## Dev Server

```bash
elm-land server  # starts this app at http:/localhost:1234
```
Requires the following environment variables to be set:

`SERVER_URL`= Url of the backend host + api entrypoint e.g. https://192.168.1.217:8008/api/v1/

## Production

Build the application

```bash
elm-land build
```
Requires the following environment variables to be set:

`SERVER_URL`= Url of the backend host + api entrypoint e.g. https://192.168.1.217:8008/api/v1/

Output is stored in the `dist/` directory.

Better instructions on deployment can be found __[here](https://elm.land/guide/deploying.html#deploying-to-production)__ at the elm-land site.

### Example with npm http-server

```bash
http-server dist/ --port 8080 -P "http://localhost:8080?" 
```

NOTE: All Elm Land apps are compiled as "single-page applications" in the dist folder. This means no matter what page is requested, that request will need to be directed to a single file: `dist/index.html`. In this example this is done with the `-P` flag.    

# TO-DO

- [ ] Add ⚠️ emote to rooms and professors with conflicts.
- [x] Use `text-decoration: underline;` to show what rooms/events/lects/blocks are hidden.
- [ ] Replace old lists in main with the ones used in the other pages
- [x] Replace Model with a record