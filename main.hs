module Main where 
    import System.IO
    import qualified Data.Map as M

    main :: IO ()
    main = run_analyzer
    
    run_analyzer :: IO ()
    run_analyzer = do 
        handle <- openFile "messages_ja.htm" ReadMode
        handle2 <- openFile "out.txt" WriteMode
        mesg_html <- (hGetContents handle)
        hPutStr handle2 (analyze mesg_html)
        hClose handle2
        putStr "Done."        


    type SmilesCounts = M.Map Char Int
    type MsgYear = Int
    type MsgMonth = Int
    type MesgsData = M.Map MsgYear (M.Map MsgMonth SmilesCounts)  --[(MsgYear, [SmilesByMonths])]
    type MsgDate = (MsgYear, MsgMonth)


    analyze :: String -> String
    analyze input = analyze_ input M.empty
    analyze_ :: String -> MesgsData -> String
    analyze_ input msgs_info = let (rest_input, msgs_info_res) = (process_message input msgs_info) in
        case rest_input of 
            Nothing -> print_results msgs_info_res
            Just rest -> (analyze_ rest) msgs_info_res
    
    print_results :: MesgsData -> String
    print_results msg_data = print_years (M.assocs msg_data) ""
     
    print_years :: [(MsgYear, M.Map MsgMonth SmilesCounts)] -> String -> String
    print_years x acc
        | x == [] = acc
        | otherwise = let (year, months):r = x in
            print_years r (acc ++ (show year) ++ "\n-------------------------\n" ++ (print_months (M.assocs months) ""))  

    print_months :: [(MsgMonth, SmilesCounts)] -> String -> String
    print_months x acc 
        | x == [] = acc
        | otherwise = let (month, smiles):r = x in
            print_months r (acc ++ (show month) ++ ":\n" ++ (print_smiles (M.assocs smiles) ""))

    print_smiles :: [(Char, Int)] -> String -> String
    print_smiles smiles acc
        | smiles == [] = acc
        | otherwise = let (smile, count):r = smiles in
            print_smiles r (acc ++ "\t" ++ smile:": " ++ show count ++ "\n")
    

    -- let x = M.fromList [(1, [('a', 4), ('b', 5)]), (2, [('a', 5), ('l', 10)])]
    -- let y = M.fromList [(2012, x), (2013, x)]

    process_message :: String -> MesgsData -> (Maybe String, MesgsData)
    process_message input msg_data = case cut_until_find "<div class=\"message_header\">" input of
        Nothing -> (Nothing, msg_data)
        Just t -> case get_msg_date t of
            (Nothing, _) -> (Nothing, msg_data)
            (Just t2, msg_date) -> 
                case get_smiles t2 of
                (Nothing, _) -> (Nothing, msg_data)
                (Just t3, smiles_counts) ->
                    let rest = cut_until_find "</p>" t3 in 
                    (rest, add_to_msg_data msg_data msg_date smiles_counts)

    get_msg_date :: String -> (Maybe String, MsgDate)
    get_msg_date s =  case cut_until_find "<span class=\"meta\">" s of
        Nothing -> (Nothing, (0, 0))
        Just t -> 
            let Just x =  cut_until_find " " t in
            let (month, Just rest) = get_text_until " " x in
            let (year, Just rest2) = get_text_until " " rest in
            (Just rest2, (read year :: Int, convertMonthToMsgMonth month ))

    add_to_msg_data :: MesgsData -> MsgDate -> SmilesCounts -> MesgsData
    add_to_msg_data msg (year, month) smiles = 
        M.insertWith r year (M.fromList [(month, smiles)] ) msg
            where r new old = M.unionWith f new old
                    where f new_s old_s = M.unionWith (+) new_s old_s


    get_smiles :: String -> (Maybe String, SmilesCounts)
    get_smiles input = case cut_until_find "<p>" input of
        Nothing -> (Nothing, M.empty)
        Just t -> let (message, rest) = get_text_until "</p>" t in 
            (rest, analyze_smiles message)

    analyze_smiles :: String -> SmilesCounts
    analyze_smiles text = analyze_smiles_ text ( M.fromList (map (\a -> (a, 0))  smiles_char ) )  

    analyze_smiles_ :: String -> SmilesCounts -> SmilesCounts
    analyze_smiles_ [] smiles = smiles 
    analyze_smiles_ (x:text) smiles = analyze_smiles_ text updated_smiles  
        where updated_smiles = add_occurence x smiles
    
    add_occurence :: Char -> SmilesCounts -> SmilesCounts
    add_occurence c smiles = M.adjust (1+) c smiles 


    smiles_char :: [Char]
    smiles_char = "😘"

    convertMonthToMsgMonth :: String -> MsgMonth
    convertMonthToMsgMonth text 
        | text == "leden" = 1
        | text == "únor" = 2
        | text == "březen" = 3
        | text == "duben" = 4
        | text == "květen" = 5
        | text == "červen" = 6
        | text == "červenec" = 7
        | text == "srpen" = 8
        | text == "září" = 9
        | text == "říjen" = 10
        | text == "listopad" = 11
        | text == "prosinec" = 12
        | otherwise = -1

    ------------------------- KMP implementation -----------------
    
    cut_until_find:: String -> String -> Maybe String
    cut_until_find word text = cut_until_find_helper word text automata (length word) 0
        where automata = get_automata word
    
    cut_until_find_helper:: String -> String -> [Int] -> Int -> Int -> Maybe String 
    cut_until_find_helper _ [] _ _ _ = Nothing
    cut_until_find_helper word (x:text_rest) automata word_length current_state =
        let next_state = (next_automata_state x (word!!(current_state)) current_state automata) in
            if next_state == word_length 
            then Just text_rest
            else (cut_until_find_helper word text_rest automata word_length next_state)

    get_text_until :: String -> String -> (String, Maybe String)
    get_text_until word text = get_text_until_helper word text (get_automata word) (length word) 0 "" --get_text_until_helper 

    get_text_until_helper:: String -> String -> [Int] -> Int -> Int -> String -> (String, Maybe String)
    get_text_until_helper _ [] _ _ _ current_text = (reverse current_text, Nothing)
    get_text_until_helper word (x:text_rest) automata word_length current_state current_text =
        let next_state =  (next_automata_state x (word!!(current_state)) current_state automata) in
            if next_state == word_length 
            then (reverse $ drop (word_length-1) current_text, Just text_rest) 
            else (get_text_until_helper word text_rest automata word_length next_state (x:current_text))
    

    next_automata_state ::  Char -> Char -> Int -> [Int] -> Int
    next_automata_state text_c word_c current_state prev_states_table
        | text_c == word_c    = current_state + 1
        | current_state == 0 = 0
        | otherwise = next_automata_state text_c word_c (prev_states_table!!current_state) prev_states_table



    -----------building backward edges ------------
    -- for word, current read char and prev status, prev_statuses table gets next status 
    get_prev_state :: String -> Char -> Int -> [Int] -> Int
    get_prev_state word c prev_status counted_statuses
        | prev_status == 0 = if (c == word!!0) then 1 else 0
        | otherwise = 
            -- one previous status back is match 
            if (( word !! prev_status) == c)
                then prev_status + 1 
                -- there's no match on previous status
                else  get_prev_state word c (counted_statuses !! ( (length counted_statuses)- prev_status)) counted_statuses 
                
    get_prev_status_table :: String -> String -> Int -> [Int] -> [Int]             
    get_prev_status_table _ [] _ _ = []
    get_prev_status_table word (x:ws) prev_state counted_statuses = 
        let status = (get_prev_state word x prev_state counted_statuses) in
            status:get_prev_status_table word ws status (status:counted_statuses)
 

    get_automata :: String -> [Int]
    get_automata word
       | word == [] = []
       | otherwise = 0:get_prev_status_table word (tail word) 0 []
    
