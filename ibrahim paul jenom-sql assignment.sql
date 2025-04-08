-- Creating HospitalDB Database
CREATE database HospitalDB;

-- Creating patients table
CREATE table patients (
patient_id int primary key,
first_name varchar (50),
last_name varchar (50),
dob date ,
phone_number varchar (15)
); 

-- Creating doctors table
CREATE table doctors (
doctor_id int primary key,
first_name varchar (50),
last_name varchar (50),
specialty varchar (100),
phone_number varchar (15)
); 

-- Creating appointments table
CREATE table appointments (
appointment_id int primary key,
patient_id int,
appointment_date date,
appointment_time time,
foreign key (patient_id) references patients (patient_id)
);

-- Creating medications table
CREATE TABLE medications (
medication_id int primary key,
patient_id int,
medication_name VARCHAR (100),
dosage VARCHAR (50),
prescribed_date DATE,
FOREIGN KEY (patient_id) REFERENCES patients (patient_id)
);

-- Creating billing table
CREATE TABLE billing (
bill_id INT primary key,
appointment_id int,
amount DECIMAL (10,2),
payment_status VARCHAR (20),
FOREIGN KEY (appointment_id) REFERENCES appointments (appointment_id)
);

-- Inserting data into patient table
INSERT INTO patients (patient_id, first_name, last_name, dob, phone_number) 
VALUES (001, 'John', 'Doe', '1985-04-12', '234-456-7890'),
(002, 'Jane', 'Smith', '1990-06-15', '234-567-8901'),
(003, 'Michael', 'Johnson', '1978-09-20', '234-678-295'),
(004, 'Emily', 'Davis', '1995-11-30', '234-789-0123'),
(005, 'Chris', 'Brown', '1988-02-10', '234-890-1234');

-- Inserting data into doctors table
INSERT INTO doctors (doctor_id, first_name, last_name, specialty, phone_number)
VALUES (01, 'Sarah', 'Williams', 'Cardiology', '234-654-3210'),
(02, 'David', 'Miller', 'Dermatology', '234-543-2109'),
(03, 'James', 'Wilson', 'Pediatrics', '234-432-1098'),
(04, 'Linda', 'Taylor', 'Orthopedics', '234-321-0987'),
(05, 'Robert', 'Anderson', 'Neurology', '234-210-9876');

-- Inserting data into the appointments table
INSERT INTO appointments (appointment_id, patient_id, doctor_id, appointment_date, appointment_time)
VALUES (1, 001, 01, '2023-01-15', '09:00:00'),
(2, 002, 02, '2023-02-20', '10:30:00'),
(3, 003, 03, '2023-03-25', '14:00:00'),
(4, 004, 04, '2023-04-10', '11:15:00'),
(5, 005, 05, '2023-05-05', '13:45:00');

-- Inserting data into medications table
INSERT INTO medications (medication_id, patient_id, medication_name, dosage, prescribed_date)
VALUES (12, 001, 'Aspirin', '500 mg', '2023-01-16'),
(23, 002, 'Ibuprofen', '200 mg', '2023-02-21'),
(34, 003, 'Amoxicillin', '250 mg', '2023-03-26'),
(45, 004, 'Metformin', '850 mg', '2023-04-11'),
(56, 005, 'Lisinopril', '10 mg', '2023-05-06');

-- Inserting data into billing table
INSERT INTO billing (bill_id, appointment_id, amount, payment_status)
VALUES (10, 1, 150.00, 'Paid'),
(20, 2, 200.00, 'Pending'),
(30, 3, 250.00, 'Paid'),
(40, 4, 300.00, 'Pending'),
(50, 5, 180.00, 'Paid');

-- Altering appointment table by adding doctor-id column
ALTER TABLE appointments 
add COLUMN doctor_id int;

-- Altering appointment table by adding foreign key
ALTER TABLE appointments
add FOREIGN KEY (doctor_id) references doctors (doctor_id);

-- Describing table to see data
DESCRIBE appointments;
DESCRIBE billing;
SELECT *
FROM patients;

-- Updating tables
INSERT INTO patients (patient_id, first_name, last_name, dob, phone_number)
VALUES (006, 'John', 'Smith', '1995-11-30', '1990-05-15');

UPDATE patients
SET phone_number = '987-654-3210'
WHERE patient_id = 006;

UPDATE patients
SET dob = '1990-05-15'
WHERE patient_id = 006;

UPDATE doctors
SET specialty = "Cardiologist"
WHERE doctor_id = 01;

UPDATE appointments
SET appointment_date = "2024-12-01"
WHERE appointment_id = 3;

-- Deleting rows and tables

DELETE FROM patients
WHERE patient_id = 4;

DELETE FROM doctors
WHERE doctor_id = 2;

DROP TABLE medications;

-- Selecting data from the HospitalDB Database
SELECT *
FROM patients
WHERE dob > '2000-01-01';
SELECT *
FROM doctors
WHERE specialty = 'Pediatrics';
SELECT * 
FROM appointments
WHERE appointment_date = '2024-11-20';

-- Sorting patients last name by ascending order
SELECT *
FROM patients
ORDER BY last_name DESC;

SELECT *
FROM medications
WHERE patient_id = 001
ORDER BY prescribed_date ASC;

SELECT first_name, last_name
FROM patients
WHERE last_name LIKE 'A%';

SELECT *
FROM appointments
WHERE appointment_date BETWEEN 2024-11-01 AND 2024-12-31;

SELECT *
FROM doctors
WHERE specialty LIKE '%surgery%';

SELECT *
FROM medications
WHERE dosage = '500 mg' OR dosage = '1000 mg';

SELECT *
FROM patients
WHERE phone_number = "123-456-7890" OR phone_number = "987-654-3210";

-- Aggregating functions on database
SELECT COUNT( DISTINCT patient_id)
FROM patients;

SELECT AVG(amount)
FROM billing;





