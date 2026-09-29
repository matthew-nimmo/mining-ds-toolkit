package dqs

import "math"

type clopeCluster struct {
	id    int
	N     int
	S     int
	W     int
	D     float64
	items map[string]int
}

func clope(transactions []map[string]int, R float64) []int {
	if len(transactions) == 0 {
		return nil
	}

	clusters := make([]*clopeCluster, 0, len(transactions))
	labels := make([]int, len(transactions))
	nextID := 1

	first := newClopeCluster(nextID, transactions[0], R)
	clusters = append(clusters, first)
	labels[0] = first.id
	nextID++

	for i := 1; i < len(transactions); i++ {
		tx := transactions[i]
		bestIdx := -1
		bestDelta := math.Inf(-1)
		for j, c := range clusters {
			delta := c.delta(tx, R)
			if delta > bestDelta {
				bestDelta = delta
				bestIdx = j
			}
		}

		newProfit := clusterProfit(1, sumTransaction(tx), len(tx), R)
		if bestIdx >= 0 && bestDelta > newProfit {
			clusters[bestIdx].addTransaction(tx, R)
			labels[i] = clusters[bestIdx].id
		} else {
			c := newClopeCluster(nextID, tx, R)
			clusters = append(clusters, c)
			labels[i] = c.id
			nextID++
		}
	}

	for pass := 0; pass < 50; pass++ {
		moved := false
		for i, tx := range transactions {
			oldID := labels[i]
			oldIdx := findClusterIndex(clusters, oldID)
			if oldIdx < 0 {
				continue
			}

			clusters[oldIdx].removeTransaction(tx, R)
			if clusters[oldIdx].N == 0 {
				clusters = append(clusters[:oldIdx], clusters[oldIdx+1:]...)
				for j := range labels {
					if labels[j] == oldID {
						labels[j] = 0
					}
				}
			}

			bestIdx := -1
			bestDelta := math.Inf(-1)
			for j, c := range clusters {
				delta := c.delta(tx, R)
				if delta > bestDelta {
					bestDelta = delta
					bestIdx = j
				}
			}

			newProfit := clusterProfit(1, sumTransaction(tx), len(tx), R)
			var chosenID int
			if bestIdx >= 0 && bestDelta > newProfit {
				clusters[bestIdx].addTransaction(tx, R)
				chosenID = clusters[bestIdx].id
			} else {
				c := newClopeCluster(nextID, tx, R)
				clusters = append(clusters, c)
				chosenID = c.id
				nextID++
			}

			if chosenID != oldID {
				moved = true
			}
			labels[i] = chosenID
		}
		if !moved {
			break
		}
	}

	idMap := make(map[int]int, len(clusters))
	newID := 1
	for _, c := range clusters {
		if _, ok := idMap[c.id]; !ok {
			idMap[c.id] = newID
			newID++
		}
	}
	for i, id := range labels {
		labels[i] = idMap[id]
	}

	return labels
}

func newClopeCluster(id int, tx map[string]int, R float64) *clopeCluster {
	c := &clopeCluster{id: id, items: make(map[string]int, len(tx))}
	c.addTransaction(tx, R)
	return c
}

func (c *clopeCluster) addTransaction(tx map[string]int, R float64) {
	for item, count := range tx {
		c.items[item] += count
	}
	c.N++
	c.S += sumTransaction(tx)
	c.W = len(c.items)
	c.D = clusterProfit(c.N, c.S, c.W, R)
}

func (c *clopeCluster) removeTransaction(tx map[string]int, R float64) {
	c.N--
	c.S -= sumTransaction(tx)
	for item, count := range tx {
		if current, ok := c.items[item]; ok {
			current -= count
			if current <= 0 {
				delete(c.items, item)
			} else {
				c.items[item] = current
			}
		}
	}
	c.W = len(c.items)
	if c.N > 0 {
		c.D = clusterProfit(c.N, c.S, c.W, R)
	} else {
		c.D = 0
	}
}

func (c *clopeCluster) delta(tx map[string]int, R float64) float64 {
	sNew := c.S + sumTransaction(tx)
	wNew := c.W
	for item := range tx {
		if _, ok := c.items[item]; !ok {
			wNew++
		}
	}
	if c.N == 0 {
		return float64(sNew) / math.Pow(float64(wNew), R)
	}
	return float64(c.N+1)*float64(sNew)/math.Pow(float64(wNew), R) - c.D
}

func clusterProfit(N, S, W int, R float64) float64 {
	if W == 0 {
		return 0
	}
	return float64(N*S) / math.Pow(float64(W), R)
}

func findClusterIndex(clusters []*clopeCluster, id int) int {
	for i, c := range clusters {
		if c.id == id {
			return i
		}
	}
	return -1
}

func sumTransaction(tx map[string]int) int {
	total := 0
	for _, count := range tx {
		total += count
	}
	return total
}