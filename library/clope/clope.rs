use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct Cluster {
    id: usize,
    items: HashMap<String, usize>, // Item frequency map
    size: usize,                   // Total items S(C)
}

impl Cluster {
    fn new(id: usize) -> Self {
        Cluster {
            id,
            items: HashMap::new(),
            size: 0,
        }
    }

    fn add_transaction(&mut self, transaction: &[String]) {
        self.size += transaction.len();
        for item in transaction {
            let count = self.items.entry(item.clone()).or_insert(0);
            *count += 1;
        }
    }

    fn remove_transaction(&mut self, transaction: &[String]) {
        self.size -= transaction.len();
        for item in transaction {
            if let Some(count) = self.items.get_mut(item) {
                if *count > 1 {
                    *count -= 1;
                } else {
                    self.items.remove(item);
                }
            }
        }
    }

    fn width(&self) -> usize {
        self.items.len()
    }

    fn profit(&self, repulsion: f64) -> f64 {
        let w = self.width();
        if w == 0 {
            return 0.0;
        }
        (self.size as f64 / w as f64).powf(repulsion) * (self.size as f64)
    }
}

struct Clope {
    repulsion: f64,
    clusters: Vec<Cluster>,
}

impl Clope {
    fn new(repulsion: f64) -> Self {
        Clope {
            repulsion,
            clusters: vec![Cluster::new(0)], // Start with one empty cluster
        }
    }

    fn fit(&mut self, transactions: &[Vec<String>]) {
        for t in transactions {
            let mut best_cluster_idx = 0;
            let mut max_profit = f64::NEG_INFINITY;

            for (idx, cluster) in self.clusters.iter().enumerate() {
                let mut temp_cluster = cluster.clone();
                temp_cluster.add_transaction(t);

                let current_profit = temp_cluster.profit(self.repulsion);
                
                // Compare with remaining base profit
                let other_profit: f64 = self.clusters.iter().enumerate().map(|(i, c)| {
                    if i == idx { 0.0 } else { c.profit(self.repulsion) }
                }).sum();

                let total_profit = current_profit + other_profit;

                if total_profit > max_profit {
                    max_profit = total_profit;
                    best_cluster_idx = idx;
                }
            }

            // Optional: Create a new cluster if it improves profit
            let mut new_cluster = Cluster::new(self.clusters.len());
            new_cluster.add_transaction(t);
            let mut temp_clusters = self.clusters.clone();
            temp_clusters.push(new_cluster);
            
            let new_total_profit: f64 = temp_clusters.iter().map(|c| c.profit(self.repulsion)).sum();

            if new_total_profit > max_profit {
                self.clusters.push(Cluster::new(self.clusters.len()));
                let last_idx = self.clusters.len() - 1;
                self.clusters[last_idx].add_transaction(t);
            } else {
                self.clusters[best_cluster_idx].add_transaction(t);
            }
        }
    }
}

fn main() {
    let mut clope = Clope::new(2.6); // Repulsion parameter usually >= 1.0

    let transactions = vec![
        vec!["a".to_string(), "b".to_string()],
        vec!["a".to_string(), "b".to_string(), "c".to_string()],
        vec!["d".to_string(), "e".to_string()],
        vec!["d".to_string(), "e".to_string(), "f".to_string()],
    ];

    clope.fit(&transactions);
    println!("Clusters found: {}", clope.clusters.len());
    for (i, c) in clope.clusters.iter().enumerate() {
        if c.size > 0 {
            println!("Cluster {}: size = {}, items = {:?}", i, c.size, c.items.keys());
        }
    }
}
