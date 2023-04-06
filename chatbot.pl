% Starts the chatbot (type "start.")
start :-
    format('Welcome! What\'s your name?~n'),
    read(Name),
    format('Hi, ~w!', [Name]),
    ask_symptom_type.

% Asks the user for the main symptom type
ask_symptom_type :-
    write('What is the main type of symptom you are experiencing?'), nl,
    write('- [general]              General sickness'), nl,
    write('- [skin]                 Affecting the Skin'), nl,
    write('- [heent]                Affecting the Head, Eyes, Ears, Nose, and Throat (HEENT)'), nl,
    write('- [pulmonary]            Affecting the Lungs'), nl,
    write('- [gastrointestinal]     Affecting the Gastrointestinal System'), nl,
    write('- [others]               Other symptoms'), nl,
    read(SymptomType), nl,
    diagnose_symptom_type(SymptomType).

diagnose_symptom_type(SymptomType) :-
    (
        type(SymptomType) -> symptom(SymptomType, Symptom), ask_symptom(Symptom);
        write('Not sure what this means, please type it again'), nl, ask_symptom_type
    ).

ask_symptom(Symptom) :-
    question(Symptom),
    write('(y/n)'), nl,
    read(Answer),
    (
        Answer == y -> assert(has(Symptom)), fail;
        assert(n(Symptom)), fail
    ).

% Diseases

%disease_symptoms(tuberculosis,).
%disease_symptoms(hiv,).
disease_symptoms(malaria, [common_symptoms, diarrhea, vomiting]).
%disease_symptoms(dengue,).
%disease_symptoms(hepatitis,).
%disease_symptoms(pneumonia,).
%disease_symptoms(leptospirosis,).
%disease_symptoms(typhoid,).
%disease_symptoms(influenza,).
%disease_symptoms(cholera,).
%disease_symptoms(covid,).

% Types

type(general).
type(skin).
type(heent).
type(pulmonary).
type(gastrointestinal).
type(others).

% Symptoms

symptom(general, fever).
symptom(general, chills).
symptom(general, headache).
symptom(general, nausea).
symptom(general, muscle_pain).
symptom(general, fatigue).

symptom(skin, rash).
symptom(skin, jaundice).

symptom(heent, sore_throat).
symptom(heent, cough).
symptom(heent, runny_nose).
symptom(heent, red_eyes).

symptom(pulmonary, cough).
symptom(pulmonary, shortness_of_breath).
symptom(pulmonary, chest_pain).

symptom(gastrointestinal, diarrhea).
symptom(gastrointestinal, vomiting).
symptom(gastrointestinal, abdominal_pain).

symptom(others, loss_of_smell).
symptom(others, loss_of_taste).

% Questions

question(fever) :- write('Are you experiencing a fever?').
question(chills) :- write('Are you experiencing chills?').
question(headache) :- write('Are you experiencing frequent headaches?').
question(nausea) :- write('Are you experiencing nausea?').
question(muscle_pain) :- write('Are you experiencing muscle pain?').
question(fatigue) :- write('Are you experiencing fatigue?').

question(rash) :- write('Do you have a rash anywhere on your body?').
question(jaundice) :- write('Do you have jaundice (yellow discoloration of the skin)?').

question(sore_throat) :- write('Do you have a sore throat?').
question(cough) :- write('Do you have a bad cough?').
question(runny_nose) :- write('Do you have a runny nose?').
question(red_eyes) :- write('Do you have red eyes?').

question(shortness_of_breath) :- write('Are you frequently experiencing shortness of breath?').
question(chest_pain) :- write('Are you experiencing any chest pain?').

question(diarrhea) :- write('Are you experiencing diarrhea?').
question(vomiting) :- write('Are you experiencing vomiting?').
question(abdominal_pain) :- write('Are you experiencing abdominal pain?').

question(loss_of_smell) :- write('Have you lost your sense of smell?').
question(loss_of_taste) :- write('Have you lost your sense of taste?').
