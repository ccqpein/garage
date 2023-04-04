use std::{collections::HashMap, time::Duration};

use chrono::{DateTime, NaiveDateTime, Utc};
use lazy_static::*;
use serde::{Deserialize, Serialize};
use tokio::{
    sync::{
        mpsc::{Receiver, Sender},
        Mutex,
    },
    time::sleep,
};
use tracing::debug;
use uuid::Uuid;

lazy_static! {
    static ref TASKS_TABLE: Mutex<HashMap<Uuid, (Task, Status)>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

#[derive(Debug)]
enum Status {
    Waiting,
    Running,
    Done,
}

#[derive(Serialize, Deserialize, Debug)]
enum TaskType {
    Fizz,
    Buzz,
    FizzBuz,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Task {
    typ: TaskType,
    exe_time: String,
}

impl Task {
    /// new task, add to "DB", set the timmer
    pub async fn new(a: Task, sender: Sender<Uuid>) -> Result<Uuid, String> {
        //let a: Self = serde_json::from_str(s).map_err(|e| e.to_string())?;
        let uuid = Uuid::new_v4();

        let now = Utc::now();
        let timestamp = DateTime::<Utc>::from_utc(
            NaiveDateTime::parse_from_str(&a.exe_time, "%Y-%m-%d %H:%M:%S")
                .map_err(|e| e.to_string())?,
            Utc,
        );
        let duration = timestamp
            .signed_duration_since(now)
            .to_std()
            .map_err(|e| e.to_string())?;

        TASKS_TABLE
            .lock()
            .await
            .insert(uuid.clone(), (a, Status::Waiting));

        tokio::spawn(async move {
            tokio::time::sleep(duration).await;
            sender.send(uuid.clone()).await.map_err(|e| e.to_string());
        });

        Ok(uuid)
    }
}

pub async fn done_the_task(id: Uuid) {
    match TASKS_TABLE.lock().await.get_mut(&id) {
        Some((_, s)) => *s = Status::Done,
        None => unreachable!(),
    }
}

pub async fn list_all_tasks() -> String {
    TASKS_TABLE
        .lock()
        .await
        .iter()
        .map(|(id, (t, s))| format!("{}: {}, {:?}\n", id, serde_json::to_string(t).unwrap(), s))
        .collect::<Vec<String>>()
        .concat()
}

pub async fn delete_task(id: Uuid) -> bool {
    TASKS_TABLE.lock().await.remove(&id).is_some()
}

pub async fn show_task(id: Uuid) -> String {
    match TASKS_TABLE.lock().await.get(&id) {
        Some((t, s)) => format!("{}: {}, {:?}\n", id, serde_json::to_string(t).unwrap(), s),
        None => String::from("nothing here"),
    }
}

pub struct Worker {
    rev: Receiver<Uuid>,
}

impl Worker {
    pub fn new(rev: Receiver<Uuid>) -> Self {
        Self { rev }
    }

    pub async fn run(&mut self) {
        while let Some(id) = self.rev.recv().await {
            match TASKS_TABLE.lock().await.get_mut(&id) {
                Some((Task { typ, exe_time }, s @ Status::Waiting)) => {
                    *s = Status::Running;
                    match typ {
                        TaskType::Fizz => {
                            tokio::spawn(async move {
                                sleep(Duration::from_secs(3)).await;
                                println!("Fizz {}", id.clone());
                                done_the_task(id).await;
                            });
                        }
                        TaskType::Buzz => {
                            tokio::spawn(async move {
                                sleep(Duration::from_secs(5)).await;
                                println!("Buzz {}", id.clone());
                                done_the_task(id).await;
                            });
                        }
                        TaskType::FizzBuz => {
                            tokio::spawn(async move {
                                println!("Fizz Buzz");
                                done_the_task(id).await;
                            });
                        }
                    }
                }
                Some((Task { typ, exe_time }, _)) => {
                    debug!("{} is done or running already, something wrong", id);
                }
                None => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Task;

    #[test]
    fn test_serde_task() {
        let t = Task {
            typ: crate::TaskType::Buzz,
            exe_time: "2023-03-31 17:30:00".to_string(),
        };

        dbg!(serde_json::to_string(&t).unwrap());

        dbg!(serde_json::from_str::<Task>(
            r#"{"typ": "Fizz", "exe_time": "2023-03-31 17:30:00"}"#
        ));
    }
}
