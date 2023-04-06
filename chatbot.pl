% Starts the chatbot (type "start." in prolog)
start :-
    format('Welcome! What\'s your name?~n'),
    read(Name),
    format('Hi, ~w!', [Name]), nl, nl,

    % Ask for general symptoms
    write('We will begin by asking you about some general symptoms'), nl,
    diagnose_symptom_type(general), nl,

    % Then asakl for the main specific symptom (Chief Complaint)
    ask_specific_symptom_type, nl,

    % Diagnosis based on HPI
    hpi(HPI),
    (
        % If not enough info on the symptoms, end
        (HPI = []; HPI = [common_symptoms]) -> write('Unfortunately we don\'t have enough information to make an accurate diagnosis, we recommend moving to a larger facility to run some tests'), nl;

        % Else, move to more specific diagnosis
        write('We will now move on to more specific diagnosis on possible diseases'), nl, nl,
        initial_diagnose_disease, 
        final_diagnose_disease,
        end
    ).

% Asks the user for the main symptom type/ chief complaints
ask_specific_symptom_type :-
    write('What is the main specific type of symptom you are experiencing?'), nl,
    write('- [skin]                 Affecting the Skin'), nl,
    write('- [heent]                Affecting the Head, Eyes, Ears, Nose, and Throat (HEENT)'), nl,
    write('- [pulmonary]            Affecting the Lungs'), nl,
    write('- [gastrointestinal]     Affecting the Gastrointestinal System'), nl,
    write('- [others]               Other symptoms'), nl,
    write('- [none]                 No more symptoms'), nl,
    read(SymptomType), nl,
    (
        SymptomType = none -> write('');
        SymptomType = general -> write('Please enter a more specific symptom type'), nl, ask_specific_symptom_type;
        diagnose_symptom_type(SymptomType)
    ).

% Diagnose the main symptom type the user entered
diagnose_symptom_type(SymptomType) :-
    (
        % If the symptom type is valid
        type(SymptomType) -> forall(symptom(SymptomType, Symptom), ask_symptom(Symptom)); % Ask about symptoms for all symptoms in that type

        %Else
        write('Not sure what this means, please type it again'), nl, ask_specific_symptom_type
    ).

% Asks the user if they are experiencing a certain symptom for a certain disease
ask_symptom_disease(Disease) :-
    disease(Disease, Symptoms),
    hpi(HPI),
    not_hpi(NHPI),
    write('Checking for remaining symptoms of: '), write(Disease), nl,
    forall((member(Symptom, Symptoms), not(member(Symptom, HPI)), not(member(Symptom, NHPI))), ask_symptom(Symptom)).

% Asks the user if they are experiencing a certain symptom
ask_symptom(Symptom) :-
    (
        % If we are diagnosing common_symptoms
        Symptom = common_symptoms -> diagnose_symptom_type(general);

        % Else
        question(Symptom),
        write('(y/n)'), nl,
        read(Answer),
        (
            Answer = y -> log_symptom(Symptom);
            Answer = n -> log_no_symptom(Symptom);
            write('Not sure what this means, please type it again'), nl, ask_symptom(Symptom)
        )
    ).

% Ask symptoms of possible diseases
initial_diagnose_disease :-
    hpi(HPI),
    forall((disease(Disease, Symptoms), subset(HPI, Symptoms)), (ask_symptom_disease(Disease), nl)).

% Just checks if there is still a possible diagnosis
has_possible_diagnosis :-
    hpi(HPI),
    disease(_, Symptoms),
    subtract(HPI, [common_symptoms], HPI2),
    subtract(Symptoms, [common_symptoms], Symptoms2),
    subset(HPI2, Symptoms2).

% Based on all the info, diagnose the disease
final_diagnose_disease :-
    hpi(HPI),
    (
        % If there is a possible diagnosis
        has_possible_diagnosis -> forall((disease(Disease, Symptoms), subset(HPI, Symptoms)), (write('Possible diagnosis: '), write(Disease), nl));
        % NEED TO ADD MORE LOGIC TO THIS PART, RN IT JUST CHECKS FOR SUBSET
        % Maybe check for the most common symptoms in the HPI and see if they match any diseases???

        % Else
        write('We don\'t have enough information or your symptoms don\'t match any known diseases in our database, we recommend more testing in a larger facility'), nl
    ).

% End
end :-
    write('Thank you for using the chatbot!').

% HPI Setup

:- dynamic hpi/1, not_hpi/1.
hpi([]).
not_hpi([]).

%Logs the symptom to the HPI
log_symptom(Symptom) :-
    hpi(HPI),
    (
        % If the symptom is a common symptom
        symptom(general, Symptom) -> (not(member(common_symptoms, HPI)) -> log_symptom(common_symptoms); true);

        % Else
        not(member(Symptom, HPI)),
        append(HPI, [Symptom], NewHPI),
        retract(hpi(HPI)),
        assert(hpi(NewHPI)),
        true
    ).

log_no_symptom(Symptom) :-
    not_hpi(NotHPI),
    (
        % If the symptom is a common symptom
        symptom(general, Symptom) -> (not(member(common_symptoms, NotHPI)) -> log_no_symptom(common_symptoms); true);

        % Else
        not(member(Symptom, NotHPI)),
        append(NotHPI, [Symptom], NewNotHPI),
        retract(not_hpi(NotHPI)),
        assert(not_hpi(NewNotHPI)),
        true
    ).
    
% Diseases

%disease(tuberculosis,).
%disease(hiv,).
disease(malaria, [common_symptoms, diarrhea, vomiting]).
disease(dengue, [common_symptoms, rash, jaundice, pain_behind_eye, blood_in_heent, blood_in_urine]).
disease(hepatitis, [common_symptoms, dark_urine, jaundice, vomiting, clay_colored_bowel_movements]).
%disease(pneumonia,).
%disease(leptospirosis,).
%disease(typhoid,).
%disease(influenza,).
%disease(cholera,).
%disease(covid,).

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
symptom(heent, pain_behind_eye).
symptom(heent, blood_in_heent).

symptom(pulmonary, cough).
symptom(pulmonary, shortness_of_breath).
symptom(pulmonary, chest_pain).

symptom(gastrointestinal, diarrhea).
symptom(gastrointestinal, vomiting).
symptom(gastrointestinal, abdominal_pain).
symptom(gastrointestinal, clay_colored_bowel_movements).

symptom(others, loss_of_smell).
symptom(others, loss_of_taste).
symptom(others, blood_in_urine).
symptom(others, dark_urine).

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
question(pain_behind_eye) :- write('Do you feel any pain behind your eyes?').
question(blood_in_heent) :- write('Do you experience nose bleeds or coughing up blood?').

question(shortness_of_breath) :- write('Are you frequently experiencing shortness of breath?').
question(chest_pain) :- write('Are you experiencing any chest pain?').

question(diarrhea) :- write('Are you experiencing diarrhea?').
question(vomiting) :- write('Are you experiencing vomiting?').
question(abdominal_pain) :- write('Are you experiencing abdominal pain?').
question(clay_colored_bowel_movements) :- write('Are your bowel movements clay colored?').

question(loss_of_smell) :- write('Have you lost your sense of smell?').
question(loss_of_taste) :- write('Have you lost your sense of taste?').
question(blood_in_urine) :- write('Have you noticed blood in your urine?').
question(dark_urine) :- write('Have you noticed your urine turning into a darker color?').
