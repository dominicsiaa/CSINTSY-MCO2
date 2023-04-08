% Starts the chatbot (type "start." in prolog)
start :-
    % Can delete tbh, I was just testing stuff here
    format('Welcome! What\'s your name?~n'),
    read(Name), nl,
    format('Hi, ~w!', [Name]), nl,

    % Ask general symptoms + specific chief complaint
    write('We will begin by asking you about some general symptoms'), nl, nl,
    ask_general_symptom, nl,
    ask_specific_symptom_type,

    % Diagnosis based on current symptoms (HPI)
    hpi(HPI), nl,
    write('Based on the information you have provided us, we have the following information:'), nl,
    forall(member(Symptom, HPI), (write('- '), desc(Symptom), nl)),
    nl,
    (
        % If not enough info on the symptoms (if the user has no symptoms, or only common sympytoms), end
        (HPI = []; not(has_possible_diagnosis)) -> write('Unfortunately we don\'t have enough information to make an accurate diagnosis, we recommend moving to a larger facility to run some tests'), nl;

        % Else, move to more specific diagnosis
        write('We will now move on to more specific diagnosis on possible diseases'), nl, nl,
        initial_diagnose_disease, 
        final_diagnose_disease,
        end
    ).

% Ask the user for presence of common symptoms
ask_general_symptom :-
    forall(symptom(general, Symptom), ask_symptom(Symptom)).

% Asks the user for the main symptom type/ chief complaint type
ask_specific_symptom_type :-
    write('What is the main specific type of symptom you are experiencing?'), nl,
    write('- [skin] \tAffecting the Skin'), nl,
    write('- [heent] \tAffecting the Head, Eyes, Ears, Nose, and Throat (HEENT)'), nl,
    write('- [pulmonary] \tAffecting the Lungs'), nl,
    write('- [gastrointestinal] \tAffecting the Gastrointestinal System'), nl,
    write('- [others] \tOther symptoms'), nl,
    write('- [none] \tNo more symptoms'), nl,
    read(SymptomType), nl,
    (
        SymptomType = none -> write('');  % If none, do nothing
        SymptomType = general -> write('Please enter a more specific symptom type'), nl, ask_specific_symptom_type; % If general, ask again
        ask_specific_symptom(SymptomType)
    ).

% Ask the user for the chief complaint based on the symptom type
ask_specific_symptom(SymptomType) :-
    (
        % If SymptomType is not a valid symptom type, ask again
        not(type(SymptomType)) -> write('Not sure what this means, please type it again'), nl, ask_specific_symptom_type;

        % Else ask the symptoms
        write('Please enter the symptom you are experiencing'), nl,
        forall(symptom(SymptomType, Symptom), (format('- [~w] \t', [Symptom]), desc(Symptom), nl)),
        write('- [other] \tThe symptom you are looking for is not here'), nl,
        read(Symptom),
        (
            % If user entered other
            Symptom = other -> write('');

            % If the symptom entered is valid, log into HPI
            symptom(SymptomType, Symptom) -> log_symptom(Symptom), nl;

            % Else, ask again
            write('Not sure what this means, please type it again'), nl, ask_specific_symptom(SymptomType)
        )
    ).

% Asks the user if they are experiencing a certain symptom
ask_symptom(Symptom) :-
    write('Are you experiencing '), desc(Symptom), write('? (y/n)'), nl,
    read(Answer),
    (
        Answer = y -> log_symptom(Symptom);
        Answer = n -> log_no_symptom(Symptom);
        write('Not sure what this means, please type it again'), nl, ask_symptom(Symptom)
    ).

% Check for remaining unasked symptoms of possible diseases
initial_diagnose_disease :-
    forall((hpi(HPI), disease(Disease, Symptoms), subset(HPI, Symptoms)), 
        (
            write('Checking for remaining symptoms of: '), write(Disease), nl,
            forall((hpi(HPI2), not_hpi(NHPI2), member(Symptom, Symptoms), not(member(Symptom, HPI2)), not(member(Symptom, NHPI2))), (ask_symptom(Symptom), true)), nl
        )
    ).

% Just checks if there is still a possible diagnosis
has_possible_diagnosis :-
    hpi(HPI),
    disease(_, Symptoms),
    findall(Symptom, symptom(general, Symptom), GeneralSymptoms),
    subtract(HPI, GeneralSymptoms, HPI2),
    subtract(Symptoms, GeneralSymptoms, Symptoms2),
    subset(HPI2, Symptoms2),
    nth0(0, HPI2, _). % Checks if the list is not empty

% Based on all the info, diagnose the disease
final_diagnose_disease :-
    hpi(HPI),
    (
        % If there is a possible diagnosis
        has_possible_diagnosis -> forall((disease(Disease, Symptoms), subset(HPI, Symptoms)), (write('Possible diagnosis: '), write(Disease), nl))
        % Else
        write('We don\'t have enough information or your symptoms don\'t match any known diseases in our database, we recommend more testing in a larger facility'), nl
    ).

% End
end :-
    write('Thank you for using the chatbot!'),
    true.

% HPI Setup
:- dynamic hpi/1, not_hpi/1. % Dynamic means we can assert/retract facts during runtime
hpi([]). % Initially, the HPI is empty
not_hpi([]). % Initially, the NotHPI is empty (for symptoms that the user does not have to avoid questions from being repeated)

%Logs the symptom to the HPI
log_symptom(Symptom) :-
    hpi(HPI),
    not(member(Symptom, HPI)),
    append(HPI, [Symptom], NewHPI),
    retract(hpi(HPI)),
    assert(hpi(NewHPI)),
    true.

log_no_symptom(Symptom) :-
    not_hpi(NotHPI),
    not(member(Symptom, NotHPI)),
    append(NotHPI, [Symptom], NewNotHPI),
    retract(not_hpi(NotHPI)),
    assert(not_hpi(NewNotHPI)),
    true.
    
% Diseases

%disease(tuberculosis,).
%disease(hiv,).
disease(malaria, [fever, chills, headache, nausea, muscle_pain, fatigue, diarrhea, vomiting]).
disease(dengue, [fever, chills, headache, nausea, muscle_pain, fatigue, diarrhea, rash, jaundice, pain_behind_eye, blood_in_heent, blood_in_urine]).
disease(hepatitis, [fever, chills, headache, nausea, muscle_pain, fatigue, diarrhea, dark_urine, jaundice, vomiting, clay_colored_bowel_movements]).
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
symptom(heent, bad_cough).
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

% Symptom Descriptions

desc(fever) :- write('Fever (high body temperature)').
desc(chills) :- write('Chills (sudden coldness and shivering)').
desc(headache) :- write('Headache (pain in the head or neck area)').
desc(nausea) :- write('Nausea (feeling of discomfort in the stomach)').
desc(muscle_pain) :- write('Muscle pain (soreness or discomfort in the muscles)').
desc(fatigue) :- write('Fatigue (feeling of tiredness or exhaustion)').

desc(rash) :- write('Rash (abnormal change in the skin, often with redness, itching, or bumps)').
desc(jaundice) :- write('Jaundice (yellowing of the skin and eyes)').

desc(sore_throat) :- write('Sore throat (pain or discomfort in the throat, often accompanied by difficulty swallowing)').
desc(bad_cough) :- write('Bad cough (excessive coughing accompanied by phlegm)').
desc(runny_nose) :- write('Runny nose (excess production of mucus in the nose)').
desc(red_eyes) :- write('Red eyes (red coloration in the eyes)').
desc(pain_behind_eye) :- write('Pain behind the eye (headache or discomfort felt around or behind the eye)').
desc(blood_in_heent) :- write('Bleeding (from the nose or gums)').

desc(shortness_of_breath) :- write('Shortness of breath (difficulty breathing)').
desc(chest_pain) :- write('Chest pain (discomfort or pressure felt in the chest)').

desc(diarrhea) :- write('Diarrhea (loose or watery bowel movements, often accompanied by abdominal cramping or discomfort)').
desc(vomiting) :- write('Vomiting (expulsion of stomach contents through the mouth)').
desc(abdominal_pain) :- write('Abdominal pain (discomfort or pain felt in the abdominal region)').
desc(clay_colored_bowel_movements) :- write('Clay-colored bowel movements (stools that are pale or grayish in color)').

desc(loss_of_smell) :- write('Loss of smell (reduced ability to detect odors)').
desc(loss_of_taste) :- write('Loss of taste (reduced ability to detect flavors)').
desc(blood_in_urine) :- write('Bleeding (presence of blood in the urine)').
desc(dark_urine) :- write('Dark urine (urine that is darker than usual)').
