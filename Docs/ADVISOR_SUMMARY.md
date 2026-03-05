# CRITICAL ISSUE SUMMARY - For Advisor Review

**Student**: Ashwin  
**Date**: February 10, 2026  
**Issue**: Data Access Limitation Blocking Dissertation Analysis

---

## The Problem (In 60 Seconds)

My dissertation requires matching **formal factories** (ASI data) with **informal suppliers** (ASUSE data) at the **district level**.

**What I discovered**:
- ✅ ASUSE (informal): Has real district codes (75 districts, 7,519 enterprises)
- ❌ ASI (formal): Districts suppressed to "1" or "2" (no real geographic info)
- **Result**: Cannot calculate district-level formal employment density
- **Impact**: Cannot run planned regressions - only 2 observations instead of 200+

---

## What This Blocks

My **research question #3**: *"Does local formal job availability affect informal wages?"*

**Planned variable**:
```
D_ij = Formal employment in district i, industry j
        ÷ Working-age population in district i
```

**Problem**: Without knowing which real district the ASI factories are in, I cannot:
1. Calculate formal job density by district
2. Match formal and informal firms in the same district
3. Test the job availability mechanism

---

## What I've Already Done (Not Wasted)

✅ Verified all variable mappings from codebooks  
✅ Identified correct item codes for wages, employment, outsourcing  
✅ Built complete data processing pipeline (all R scripts working)  
✅ Validated 8,137 factories + 7,519 enterprises (good sample sizes)

**Everything is ready** - just missing geographic identifiers in ASI data.

---

## Three Options

### Option A: Request Restricted ASI Data (Original Plan)
- Apply to MOSPI for unit-level data with real district codes
- **Timeline**: 3-6 months approval process
- **Viable for**: PhD (if I have 6+ months)
- **Risk**: Not guaranteed approval

### Option B: Pivot to State-Industry Analysis
- Use state-level aggregation instead of district
- **Sample size**: ~60-80 obs (if ASI reveals states)
- **Trade-off**: Drop "job availability" research question
- **Viable for**: Master's on tight timeline

### Option C: ASUSE-Only Analysis (New Research Question)
- Study informal wage determinants using 7,519 enterprises
- **No formal sector needed** (ASUSE has districts)
- **Trade-off**: Completely different research question
- **Sample size**: 7,519 observations across 59 districts

---

## My Question for You

**Given my timeline and degree type, which path should I pursue?**

1. Should I apply for restricted ASI data? (requires 3-6 months)
2. Can I pivot to state-level or ASUSE-only analysis?
3. Are there alternative formal sector data sources I should consider?

---

## Supporting Documentation

Full technical report: [`DATA_LIMITATION_REPORT.md`](file:///c:/Users/ashwin/Documents/Formal_Informal/DATA_LIMITATION_REPORT.md)

All R scripts and verified mappings available in project folder.
