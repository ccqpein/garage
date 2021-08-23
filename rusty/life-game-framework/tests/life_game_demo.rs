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
    // s.iter_mut()
    //     .filter(|n| *n.get_node_val() == '#')
    //     .for_each(|n| println!("{:?}", n.current_index()));

    let new_step: Vec<(Vec<usize>, char)> = s
        .iter()
        .filter_map(|n| {
            let count = n.neighbour().filter(|nei| *nei.get() == '#').count();
            if !(count == 2 || count == 3) && *n.get_node_val() == '#' {
                //n.set_node('.');
                Some((n.current_index().clone(), '.'))
            } else if *n.get_node_val() == '.' && count == 3 {
                //n.set_node('#');
                Some((n.current_index().clone(), '#'))
            } else {
                None
            }
        })
        .collect();

    new_step
        .iter()
        .for_each(|(ind, v)| s.set_node(ind, *v).unwrap());

    // check
    s.iter()
        .filter(|n| *n.get_node_val() == '#')
        .for_each(|n| println!("{:?}", n.current_index()));

    // count
    assert_eq!(11, s.iter().filter(|n| *n.get_node_val() == '#').count())
}
