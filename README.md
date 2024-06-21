# Veterinary Hospital Simulation Project

## Overview
Simulation model of the University Veterinary Hospital using R and the Simmer package to analyze and improve efficiency.

## System Description
- **Processes**: Handles emergency services, routine treatments, and cosmetic treatments.
- **Entities**: Pets, wild animals, and horses.

## Key Components
- **Reception**: Initial registration of animals.
- **Emergency Vet**: Handles urgent care and potential surgeries.
- **Routine Treatment**: Regular check-ups and vaccinations.
- **Cosmetic Treatment**: Grooming and other non-medical treatments.
- **Database**: Logs details of treatments and waits.

## Goals
- Improve efficiency by reducing wait times and optimizing resource allocation.
- Enhance customer satisfaction and hospital profitability.

## Simulation Details
- **Model**: Built using Simmer in R.
- **Metrics**: Average wait times, queue lengths, and payments.

## How to Run
1. Load the necessary R packages (`simmer`, `dplyr`, etc.).
2. Initialize the simulation with `reset(ZooHospital)` and run with `run(until=simulationTime)`.
3. Analyze results using provided functions and plots.

## Submission Notes
- Include all scripts and data files.
- Document code with comments.
- Package the project directory into a ZIP file for submission.
