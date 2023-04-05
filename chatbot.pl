start :-
    format('Welcome! What\'s your name?~n'),
    read(Name),
    format('Hi, ~w!', [Name]),
    ask_symptom_type.

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
        SymptomType == general ->
            %TODO
            write('General Symptom');

        SymptomType == skin ->
            %TODO
            write('Skin');

        SymptomType == heent ->
            %TODO
            write('Head, Eyes, Ears, Nose, and Throat (HEENT)');

        SymptomType == pulmonary ->
            %TODO
            write('Lungs');

        SymptomType == gastrointestinal ->
            %TODO
            write('Gastrointestinal');

        SymptomType == others ->
            %TODO
            write('Other Symptoms');


        write('Not sure what this means, please type it again'), nl, ask_symptom_type
    ).

% Diseases

disease(tuberculosis).
disease(hiv).
disease(malaria).
disease(dengue).
disease(hepatitis).
disease(pneumonia).
disease(leptospirosis).
disease(typhoid).
disease(influenza).
disease(cholera).
disease(covid).

% Symptoms