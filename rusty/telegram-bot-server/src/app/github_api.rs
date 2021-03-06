use chrono::{DateTime, Datelike, Duration, TimeZone, Utc};
use chrono_tz::America::New_York;
use octocrab::{initialise, instance, Octocrab, OctocrabBuilder, Result};
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use tracing::info;

async fn get_users_recently_repos(
    client: &Octocrab,
    username: &str,
) -> Result<Vec<octocrab::models::Repository>> {
    client
        .get(
            format!("/users/{}/repos", username),
            Some(&[
                ("type", "owner"),
                ("sort", "updated"),
                ("per_page", "5"),
                ("page", "1"),
            ]),
        )
        .await
}

async fn if_repo_has_commit_since(
    client: &Octocrab,
    username: &str,
    since: DateTime<Utc>,
) -> Result<bool, String> {
    let repos = get_users_recently_repos(client, username)
        .await
        .map_err(|e| e.to_string())?;
    for repo in repos {
        if repo.updated_at.ok_or("no update_at value")? - since > Duration::seconds(0) {
            return Ok(true);
        }
    }
    Ok(false)
}

async fn the_start_of_today_in_utc() -> Result<DateTime<Utc>, String> {
    let now_utc = Utc::now();
    let now_eastern = now_utc.with_timezone(&New_York);

    let dt = New_York
        .ymd(now_eastern.year(), now_eastern.month(), now_eastern.day())
        .and_hms(0, 0, 0);

    Ok(dt.with_timezone(&Utc))
}

pub(super) async fn does_this_user_have_commit_today(
    username: &str,
    token: Option<impl AsRef<Path>>,
) -> Result<bool, String> {
    let client = match token {
        Some(p) => {
            let f = BufReader::new(File::open(p).map_err(|e| e.to_string())?);
            let l = f
                .lines()
                .next()
                .ok_or("Read token line failed".to_string())?
                .map_err(|e| e.to_string())?;
            initialise(OctocrabBuilder::new().personal_token(l)).map_err(|e| e.to_string())?
        }
        None => instance(),
    };

    let since = the_start_of_today_in_utc().await?;
    info!("check if you commit since {}", since);
    if_repo_has_commit_since(&client, username, since).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{Local, NaiveDate, NaiveDateTime, NaiveTime, SecondsFormat};
    use std::time::SystemTime;
    use tokio::runtime::Runtime;

    #[test]
    fn test_github_api_call() {
        let rt = Runtime::new().unwrap();
        let client = instance();

        let re = rt
            .block_on(get_users_recently_repos(&client, "ccqpein"))
            .unwrap();
        //dbg!(&re[0]);
        assert_eq!(re.len(), 5)
    }

    #[test]
    fn test_time_duration_sub() {
        let now = SystemTime::now();
        let a = <DateTime<Utc> as From<SystemTime>>::from(now);
        dbg!(a);
        let b = a + Duration::seconds(1);
        dbg!(b);
        assert!(a - b < Duration::seconds(0));
    }

    #[test]
    fn test_make_start_of_today_local_in_utc() {
        let data = NaiveDate::from_ymd(2021, 3, 17);
        let time = NaiveTime::from_hms(0, 0, 0);
        let dt: NaiveDateTime = data.and_time(time);

        let date_time: DateTime<Local> = Local.from_local_datetime(&dt).unwrap();
        assert_eq!(
            date_time
                .with_timezone(&Utc)
                .to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-03-17T04:00:00Z"
        );

        // test winter time
        let data = NaiveDate::from_ymd(2021, 2, 25);
        let time = NaiveTime::from_hms(0, 0, 0);
        let dt: NaiveDateTime = data.and_time(time);

        let date_time: DateTime<Local> = Local.from_local_datetime(&dt).unwrap();
        assert_eq!(
            date_time
                .with_timezone(&Utc)
                .to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-02-25T05:00:00Z"
        );

        //
        let data = NaiveDate::from_ymd(2021, 3, 22);
        let time = NaiveTime::from_hms(0, 0, 0);
        let dt: NaiveDateTime = data.and_time(time);

        let date_time: DateTime<Local> = Local.from_local_datetime(&dt).unwrap();
        assert_eq!(
            date_time
                .with_timezone(&Utc)
                .to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-03-22T04:00:00Z"
        );
        assert_eq!(
            date_time
                .with_timezone(&New_York)
                .to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-03-22T00:00:00-04:00"
        );
    }

    #[test]
    fn test_make_start_of_today_est_in_utc() {
        let date_time: DateTime<Utc> = Utc.ymd(2021, 2, 25).and_hms(5, 0, 0);
        let est = date_time.with_timezone(&New_York);

        assert_eq!(
            est.to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-02-25T00:00:00-05:00"
        );

        let date_time: DateTime<Utc> = Utc.ymd(2021, 2, 25).and_hms(4, 59, 59);
        let est = date_time.with_timezone(&New_York); // one day before
        let new_york_d = New_York
            .ymd(est.year(), est.month(), est.day())
            .and_hms(0, 0, 0)
            .with_timezone(&Utc);

        assert_eq!(
            new_york_d.to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-02-24T05:00:00Z"
        );

        // summer time
        let date_time: DateTime<Utc> = Utc.ymd(2021, 5, 25).and_hms(4, 0, 0);
        let est = date_time.with_timezone(&New_York);

        assert_eq!(
            est.to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-05-25T00:00:00-04:00"
        );

        let date_time: DateTime<Utc> = Utc.ymd(2021, 5, 25).and_hms(3, 59, 59);
        let est = date_time.with_timezone(&New_York);
        let new_york_d = New_York
            .ymd(est.year(), est.month(), est.day())
            .and_hms(0, 0, 0)
            .with_timezone(&Utc);

        assert_eq!(
            new_york_d.to_rfc3339_opts(SecondsFormat::Secs, true),
            "2021-05-24T04:00:00Z"
        );
    }
}
