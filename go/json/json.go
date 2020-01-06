package main

import (
    "fmt"
    "io/ioutil"
    "encoding/json"
)

type CharacterList struct {
    Characters []Character
}

type Character struct {
    Name string
    Realm string
}

func main() {
    dat, err := ioutil.ReadFile("api_outputs/wow_user_characters.json")
    if err != nil {
        panic(err)
    }

    var characters CharacterList
    err = json.Unmarshal(dat, &characters)
    if (err != nil) {
        panic(err)
    }

    fmt.Printf("%+v", characters.Characters)
    for _, character := range characters.Characters {
        fmt.Printf("%+v\n", character)
    }

}
