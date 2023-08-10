# schedule
Front-end page built with [elm-spa](https://elm-spa.dev).

An important practical application at FCUP concerns the preparation of weekly timetables for students, professors and rooms. There is an application being used at DCC for this, but the interface is rather poor, making timetable preparation a painful task.  The aim of this project is to provide a well designed graphical users interface for timetabling, with the specific application at FCUP in view.  Usability should be enlarged for all the departments, with swift resource sharing.  Ideally, this project will deliver a complete prototype.

## dependencies

- [nodejs](https://nodejs.org/en) 
- [elm](https://elm-lang.org/)
- [elm-spa](https://github.com/ryannhg/elm-spa) 

## running locally

```bash
elm-spa server  # starts this app at http:/localhost:1234
``` 

## deployment 
The deployment configuration can be altered [here](src/DeployEnv.elm).

### other commands

```bash
elm-spa add    # add a new page to the application
elm-spa build  # production build
elm-spa watch  # runs build as you code (without the server)
```

## learn more

You can learn more at [elm-spa.dev](https://elm-spa.dev)