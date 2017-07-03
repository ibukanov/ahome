package main

import (
    "fmt"
    "os"
    "path/filepath"
)

type actionInfo struct {
    name string
    action func(actionName string, args []string)
}

type ActionData struct {
    RunStatus *RunStatus
    Name string
    Args []string
}

func (ad *ActionData) UsageErrf(format string, a ...interface{}) {
    progname := filepath.Base(os.Args[0])
    var name, helpPrefix string
    if ad.Name == "" {
        name = progname
        helpPrefix = progname
    } else {
        name = ad.Name
        helpPrefix = progname + " " + ad.Name
    }
    fmt.Fprintf(os.Stderr, "%s: %s\nTry '%s --help' for more information\n",
        name, err, helpPrefix,
    )
    ad.RunStatus.SetErrors()
}

func main() {

    ad := &ActionData{
        runStatus: NewRunStatus(),
    }
    process(ad)
    if ad.RunStatus.Errors() {
        os.Exit(1)
    }
}

func process(ad *ActionData) {
    actions := []actionInfo{
        {"check-pass", CheckPass},
    }

    if len(os.Args) < 2 {
        return ad.UsageErrf("action must be given")
    }

    i := 0
    for ;; i++ {
        if i == len(actions) {
            return ad.UsageErrf("unknown action name - %s", os.Args[1])
        }
        if actions[i].name == os.Args[1] {
            break
        }
    }

    ad.Name = actions[i].name
    ad.args = os.Args[2:]
    actions[i].action(ad)
}
