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
    hpi(HPI),
    (
        % If not enough info on the symptoms (if the user has no symptoms, or only common sympytoms), end
        (HPI = []; HPI = [common_symptoms]) -> write('Unfortunately we don\'t have enough information to make an accurate diagnosis, we recommend moving to a larger facility to run some tests'), nl;

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
        forall(symptom(SymptomType, Symptom), (format('[~w] \t', [Symptom]), desc(Symptom), nl)),
        read(Symptom),
        (
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
    hpi(HPI), not_hpi(NHPI),
    forall((disease(Disease, Symptoms), subset(HPI, Symptoms)), 
        (
            write('Checking for remaining symptoms of: '), write(Disease), nl,
            forall((member(Symptom, Symptoms), not(member(Symptom, HPI)), not(member(Symptom, NHPI))), (ask_symptom(Symptom), true)), nl
        )
    ).

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
:- dynamic hpi/1, not_hpi/1. % Dynamic means we can assert/retract facts during runtime
hpi([]). % Initially, the HPI is empty
not_hpi([]). % Initially, the NotHPI is empty (for symptoms that the user does not have to avoid questions from being repeated)

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
symptom(general, weakness_fatigue).
symptom(general, nausea).
symptom(general, muscle_body_pain).
symptom(general, sweats).
symptom(general, weight_loss).
symptom(general, loss_of_apetite).
symptom(general, malaise).
symptom(general, other_symptom).

symptom(skin, rash).
symptom(skin, jaundice).
symptom(skin, other_symptom).

symptom(heent, headache).
symptom(heent, confusion).
symptom(heent, red_eyes).
symptom(heent, jaundice).
symptom(heent, pain_behind_eye).
symptom(heent, cough).
symptom(heent, cough_blood_sputum).
symptom(heent, blood_in_heent).
symptom(heent, sore_throat).
symptom(heent, runny_stuffy_nose).
symptom(heent, dehydration).
symptom(heent, loss_of_taste_smell).
symptom(heent, other_symptom).

symptom(pulmonary, chest_pain).
symptom(pulmonary, shortness_of_breath).
symptom(pulmonary, rapid_breathing).
symptom(pulmonary, other_symptom).

symptom(gastrointestinal, diarrhea).
symptom(gastrointestinal, profuse_watery_diarrhea).
symptom(gastrointestinal, constipation).
symptom(gastrointestinal, vomiting).
symptom(gastrointestinal, abdominal_pain).
symptom(gastrointestinal, clay_colored_bowel_movements).
symptom(gastrointestinal, other_symptom).

symptom(others, blood_in_urine).
symptom(others, dark_urine).
symptom(others, irritability).
symptom(others, leg_cramps).
symptom(others, other_symptom).

% General Symptoms
desc(fever) :- write('Fever (high body temperature)').
desc(chills) :- write('Chills (shivering or feeling cold despite a fever)').
desc(weakness_fatigue) :- write('Weakness or fatigue (feeling tired and having low energy levels)').
desc(nausea) :- write('Nausea (feeling like you need to vomit)').
desc(muscle_body_pain) :- write('Muscle or body pain (aching muscles or general discomfort throughout the body)').
desc(sweats) :- write('Sweats (sudden sweating)').
desc(weight_loss) :- write('Weight loss (unintentional lost of weight)').
desc(loss_of_apetite) :- write('Loss of appetite (decreased desire to eat)').
desc(malaise) :- write('Malaise (general feeling of discomfort or unease)').
desc(other_symptom) :- write('Other symptoms (symptoms not listed above)').

% Skin Symptoms
desc(rash) :- write('Rash (abnormal change in the color or texture of the skin)').
desc(jaundice) :- write('Jaundice (yellowing of the skin or whites of the eyes)').

% Head, Eyes, Ears, Nose, and Throat (HEENT) Symptoms
desc(headache) :- write('Headache (pain or pressure in the head)').
desc(confusion) :- write('Confusion (difficulty thinking or understanding)').
desc(red_eyes) :- write('Red eyes (bloodshot or irritated eyes)').
desc(pain_behind_eye) :- write('Pain behind the eyes (pain or discomfort felt behind the eyes)').
desc(cough) :- write('Cough (sudden, forceful expulsion of air from the lungs)').
desc(cough_blood_sputum) :- write('Cough with blood or sputum (coughing up blood or phlegm)').
desc(blood_in_heent) :- write('Blood in HEENT (bleeding from the nose, mouth, or throat)').
desc(sore_throat) :- write('Sore throat (pain, scratchiness or irritation of the throat)').
desc(runny_stuffy_nose) :- write('Runny or stuffy nose (nasal congestion or discharge)').
desc(dehydration) :- write('Dehydration (excessive loss of fluids and electrolytes)').
desc(loss_of_taste_smell) :- write('Loss of taste or smell (partial or complete loss of taste or smell)').

% Pulmonary Symptoms
desc(chest_pain) :- write('Chest pain (pain, pressure or discomfort in the chest)').
desc(shortness_of_breath) :- write('Shortness of breath (difficulty breathing or feeling like you cannot get enough air)').
desc(rapid_breathing) :- write('Rapid breathing (breathing faster than normal)').

% Gastrointestinal Symptoms
ddesc(diarrhea) :- write('Diarrhea (frequent loose, watery bowel movements)').
desc(profuse_watery_diarrhea) :- write('Profuse watery diarrhea (uncontrollable diarrhea that can quickly lead to severe dehydration)').
desc(constipation) :- write('Constipation (difficulty passing stools or infrequent bowel movements)').
desc(vomiting) :- write('Vomiting (forceful expulsion of stomach contents through the mouth)').
desc(abdominal_pain) :- write('Abdominal pain (pain or discomfort in the area between the chest and pelvis)').
desc(clay_colored_bowel_movements) :- write('Clay-colored bowel movements (stool that is pale or grayish in color)').

%Other Symptomps
desc(blood_in_urine) :- write('Blood in urine (hematuria, pink, red, or brown urine)').
desc(dark_urine) :- write('Dark urine (urine that is darker than usual, such as brown, amber, or tea-colored urine)').
desc(irritability) :- write('Irritability (a tendency to become easily annoyed or agitated)').
desc(leg_cramps) :- write('Leg cramps (sudden and involuntary contractions of muscles in the legs)').
