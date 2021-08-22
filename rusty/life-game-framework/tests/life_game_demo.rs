use life_game_framework::map::*;

#[test]
/// https://adventofcode.com/2020/day/17
fn AOC2020_day17_demo() {
    let mut s = Space::new(3, 100, 1, '.');
    /*
       50
    50 .  #  .
    51 .  .  #
    52 #  #  #

     */
    s.get_mut([50, 51, 50])
        .map(|l| l.as_node_mut().unwrap())
        .unwrap()
        .set('#');

    s.get_mut([51, 52, 50])
        .map(|l| l.as_node_mut().unwrap())
        .unwrap()
        .set('#');

    s.get_mut([52, 50, 50])
        .map(|l| l.as_node_mut().unwrap())
        .unwrap()
        .set('#');

    s.get_mut([52, 51, 50])
        .map(|l| l.as_node_mut().unwrap())
        .unwrap()
        .set('#');

    s.get_mut([52, 52, 50])
        .map(|l| l.as_node_mut().unwrap())
        .unwrap()
        .set('#');

    // map run ruler here
    //:= TODO: here
    s.iter_mut()
        .filter(|n| *n.get_node_val() == '#')
        .for_each(|n| println!("{:?}", n.current_index()));

    // check
    s.iter()
        .filter(|n| *n.get_node_val() == '#')
        .for_each(|n| println!("{:?}", n.current_index()))
}
