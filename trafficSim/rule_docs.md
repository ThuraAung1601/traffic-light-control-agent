### **Rule 1 – Cycle Pattern (Fallback)**

If no smart decision can be made, the system simply moves to the **next pattern in sequence**.
If the current pattern number is 12, it wraps around and goes back to 1.
→ *“If you don’t know what to do, just rotate through the patterns.”*

---

### **Rule 2 – Turn-Heavy Traffic**

If a direction has a **large queue (more than 8 vehicles)** and **over 35% of them want to turn**,
then give priority to that direction’s **dedicated turn pattern**.
→ *“If many cars in one direction want to turn, let that direction go.”*

---

### **Rule 3 – Balanced Cross Traffic (Combination Patterns)**

When the queues in opposite directions (north/south and east/west) are roughly balanced,
choose a **combination pattern** that serves **two non-conflicting directions** at once.
→ *“If traffic is fairly even on both sides, serve two directions together.”*

---

### **Rule 4 – Main Intelligent Pattern Selection**

This is the **core decision rule** that decides which pattern to activate next.
It looks at queue lengths and turning demands in all four directions (N, S, E, W).

The decision logic goes in this priority order:

1. If any direction has **turn-heavy traffic**, handle that first (use Rule 2).
2. If total traffic is **balanced and high**, pick a **combination pattern** (use Rule 3).
3. If both NS and EW have **heavy traffic**, also pick a combination pattern.
4. If one axis (NS or EW) has **much more traffic** (1.5× more), give it **full green** (Pattern 11 or 12).
5. If one axis is **slightly more dominant**, use straight-only patterns (1 or 2).
6. If nothing special applies, fall back to cycling (Rule 1).
   → *“Pick the next light pattern based on where the traffic and turns are heaviest, balancing fairness and flow.”*

---

### **Rule 5 – Decision Wrapper (Integration with Python)**

This is the **main entry point** that collects all the latest data (queues, turn demands) from the simulation,
then calls **Rule 4** to select the next pattern.
→ *“Get all traffic data and ask the main rule which pattern should run next.”*

---

### **Rule 6 – Early Pattern Termination**

If the **currently served directions** have **no cars waiting**,
the system can **end the green light early**.
→ *“If nobody is waiting in the directions that are green, switch sooner.”*

---

### **Rule 7 – Pattern Performing Well**

If a pattern is performing efficiently (queue still big, cars entering slower than being served),
the system recognizes that it’s **working well** and may keep it longer.
→ *“If this green pattern is helping clear a heavy queue, let it continue.”*

---

### **Rule 8 – Time Extension Decision**

If during the first 60 seconds, the **queue keeps growing** (cars arriving faster than leaving),
and the pattern’s total green time is still below the maximum limit,
then **extend the green time by 60 frames**.
→ *“If the queue is getting longer even while green, extend the light duration.”*

---

### **Rule 9 – Maximum Time Override**

If a pattern has already been active **longer than the allowed maximum green time**,
force a change to the next pattern for safety.
→ *“If the light has been green too long, change it no matter what.”*

---

### 🔹 Summary of Rule Purposes

| Rule | Purpose       | Plain Meaning                            |
| ---- | ------------- | ---------------------------------------- |
| 1    | Fallback      | Keep lights cycling when unsure          |
| 2    | Turn Handling | Prioritize turn-heavy directions         |
| 3    | Balance       | Serve two balanced directions at once    |
| 4    | Main Logic    | Smart selection using queue & turn data  |
| 5    | Wrapper       | Interface to call main logic from system |
| 6    | Early Stop    | End green early if no cars waiting       |
| 7    | Performance   | Keep going if the green is effective     |
| 8    | Extension     | Lengthen green if queue keeps growing    |
| 9    | Safety        | Force change if max green time exceeded  |

---