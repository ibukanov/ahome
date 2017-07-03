package main

import (
    "flag"
    "fmt"
    "os"

    "golang.org/x/crypto/ssh/terminal"
)

type CheckPassArgs struct {
    setNew bool
    path string
    purposeHint string
}

func CheckPass(ad *ActionData) {
    fs := flag.NewFlagSet(actionName, flag.ContinueOnError)

    args := &CheckPassArgs{}

    fs.BoolVar(&args.setNew, "new-password", false, "set new password")
    fs.StringVar(&args.purposeHint, "hint", false, "short description of password purpose")

    err := fs.Parse(argSlice)
    if err != nil {
        return ad.UsageErrf("%s", err)
    }

    argSlice = fs.Args()
    if len(args) != 1 {
        return ad.UsageErrf("%s requires exactly one argument", actionName)
    }
    args.path := argSlice[0]
    if args.setNew {
        setPassword(ad.RunStatus, args)
    } else {
        checkPassword(ad.RunStatus, args)
    }
}

func setNewPassword(rs *RunStatus, args *CheckPassArgs) {
    _, err = fmt.Fprintf(os.Stdout, "You are about to set a new password hash to verify the password for %s, yes? ", args.purposeHint);
    if err != nil {
        return rs.Err(err)
    }

    reply := make([]byte, 81)
    n, err := os.Stdin.Read(reply)
    if err == io.EOF {
        err = nil
    }
    if err != nil {
        return rs.Err(err)
    }


    panic("NOT IMPLEMENTED")
}

func checkPassword(rs *RunStatus, args *CheckPassArgs) {
    panic("NOT IMPLEMENTED")
}
