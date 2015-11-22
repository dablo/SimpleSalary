namespace SimplePay
    // Rörlig lön = (RörligLöneKonstant*procent)*((timpris*timmar)/(TimprisKonstant*DebiterbaraTimmar))

module domain =
    let RörligLöneKonstant = 33000.0
    let GrundLöneKonstant = 20000.0
    let TimprisKonstant = 800.0
    let TimmarIMånaden = 165.0
    let SlackKonstant = 0.94
    let DebiterbaraTimmar procent = (TimmarIMånaden-8.0) * SlackKonstant * procent
    let RörligLön (procent, timpris, timmar) = (RörligLöneKonstant * procent) * ((timpris * timmar) / (TimprisKonstant * DebiterbaraTimmar(procent)))
    let LöneKalkulator (procent, timpris, timmar, avdrag) = (GrundLöneKonstant * procent) + RörligLön(procent, timpris, timmar) - avdrag

module tests =

    open Xunit
    open System

    [<Fact>]
    let working_fulltime_with_800_an_hour_pays_53000()=
        let procent = 1.0
        let timpris = 800.0
        let timmar = domain.DebiterbaraTimmar(procent)
        let avdrag = 0.0
        Assert.Equal(53000, domain.LöneKalkulator(procent, timpris, timmar, avdrag) |> int)

    [<Fact>]
    let working_fulltime_with_850_an_hour_pays_55063()=
        let procent = 1.0
        let timpris = 850.0
        let timmar = domain.DebiterbaraTimmar(procent)
        let avdrag = 0.0
        Assert.Equal(55063, domain.LöneKalkulator(procent, timpris, timmar, avdrag) |> int)

    [<Fact>]
    let working_fulltime_with_900_an_hour_pays_57125()=
        let procent = 1.0
        let timpris = 900.0
        let timmar = domain.DebiterbaraTimmar(procent)
        let avdrag = 0.0
        Assert.Equal(57125, domain.LöneKalkulator(procent, timpris, timmar, avdrag) |> int)

    [<Fact>]
    let working_no_time_with_800_an_hour_pays_20000()=
        let procent = 1.0
        let timpris = 800.0
        let timmar = 0.0 // ledig
        let avdrag = 0.0
        Assert.Equal(20000, domain.LöneKalkulator(procent, timpris, timmar, avdrag) |> int)

    [<Fact>]
    let working_80_procent_with_850_an_hour_and_being_sick_one_day_pays_42158()=
        let procent = 0.8
        let timpris = 850.0
        let timmar = domain.DebiterbaraTimmar(procent) - 8.0 // sjuk en dag
        let avdrag = 0.0
        Assert.Equal(42149, domain.LöneKalkulator(procent, timpris, timmar, avdrag) |> int)

    [<Fact>]
    let working_80_procent_with_850_an_hour_and_being_sick_one_day_while_playing_with_the_new_phone_pays_45822()=
        let procent = 0.8
        let timpris = 850.0
        let timmar = domain.DebiterbaraTimmar(procent) - 8.0 // sjuk en dag
        let avdrag = 7345.0
        Assert.Equal(34804, domain.LöneKalkulator(procent, timpris, timmar, avdrag) |> int)
