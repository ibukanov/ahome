package main

import (
    "os"
)

type RunStatus struct {
    Canceled bool

    errors bool
    logError func(err error)
}

RunStatus *NewRunStatus() {
    return &RunStatus{}
}

RunStatus *NewRunStatus() {
    return &RunStatus{}
}

func (rs *RuStatus) Errors() bool {
    return rs.errors
}

func (rs *RuStatus) SetErrors() {
    rs.Canceled = true
    rs.errors = true
}


func (rs *RunStatus) Err(err error) {
    rs.Canceled = true
    rs.errors = true
    if rs.logError != nil {
        rs.logError(err)
    } else {
        os.Stderr.Println(err)
    }
}

func (rs *RunStatus) Errf(format string, a ...interface{}) {
    rs.Err(fmt.Errf(format, a...))
}

