import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Result "mo:base/Result";

actor VotingSystem {
    //type definitions

    type PollId = Nat;
    type UserId = Principal;
    
    type Poll = {
        id: PollId;
        creator: UserId;
        question: Text;
        options: [Text];
        startTime: Time.Time;
        endTime: Time.Time;
        isActive: Bool;
    };

    type Vote = {
        pollId: PollId;
        voter: UserId;
        optionIndex: Nat;
        timestamp: Time.Time;
    };

    type Error = {
        #PollNotFound;
        #PollEnded;
        #AlreadyVoted;
        #InvalidOption;
        #Unauthorized;
        #InvalidTimeRange;
    };

    // state variables
    private stable var nextPollId: PollId = 0;
    private var polls = HashMap.HashMap<PollId, Poll>(0, Nat.equal, Hash.hash);
    private var votes = HashMap.HashMap<Text, Vote>(0, Text.equal, Text.hash);
    private var pollVotes = HashMap.HashMap<PollId, [Vote]>(0, Nat.equal, Hash.hash);

    // function to combine a poll id and a voter id into a unique string key
    private func makeVoteKey(pollId: PollId, voter: UserId): Text {
        return Nat.toText(pollId) # Principal.toText(voter);
    };

    // creating a poll
    public shared(msg) func createPoll(
        question: Text,
        options: [Text],
        startTime: Time.Time,
        endTime: Time.Time
    ): async Result.Result<PollId, Error> {
        let caller = msg.caller;
        
        if (startTime >= endTime) {
            return #err(#InvalidTimeRange);
        };

        let pollId = nextPollId;
        nextPollId += 1;

        let newPoll: Poll = {
            id = pollId;
            creator = caller;
            question = question;
            options = options;
            startTime = startTime;
            endTime = endTime;
            isActive = true;
        };

        polls.put(pollId, newPoll);
        #ok(pollId)
    };

    // casting a vote
    public shared(msg) func castVote(pollId: PollId, optionIndex: Nat): async Result.Result<(), Error> {
        let caller = msg.caller;
        
        switch (polls.get(pollId)) {
            case null { return #err(#PollNotFound); };
            case (?poll) {
                let currentTime = Time.now();
                if (currentTime < poll.startTime or currentTime > poll.endTime) {
                    return #err(#PollEnded);
                };

                if (optionIndex >= poll.options.size()) {
                    return #err(#InvalidOption);
                };

                let voteKey = makeVoteKey(pollId, caller);
                switch (votes.get(voteKey)) {
                    case (?_) { return #err(#AlreadyVoted); };
                    case null {
                        let newVote: Vote = {
                            pollId = pollId;
                            voter = caller;
                            optionIndex = optionIndex;
                            timestamp = currentTime;
                        };

                        votes.put(voteKey, newVote);

                        switch (pollVotes.get(pollId)) {
                            case null {
                                pollVotes.put(pollId, [newVote]);
                            };
                            case (?existingVotes) {
                                pollVotes.put(pollId, Array.append(existingVotes, [newVote]));
                            };
                        };

                        #ok(())
                    };
                }
            };
        }
    };

    // querying data
    public query func getPoll(pollId: PollId): async Result.Result<Poll, Error> {
        switch (polls.get(pollId)) {
            case null { #err(#PollNotFound) };
            case (?poll) { #ok(poll) };
        }
    };

    
    public query func getPollResults(pollId: PollId): async Result.Result<[(Nat, Nat)], Error> {
        switch (polls.get(pollId)) {
            case null { return #err(#PollNotFound); };
            case (?poll) {
                let voteCount = Array.init<Nat>(poll.options.size(), 0);
                
                switch (pollVotes.get(pollId)) {
                    case null { };
                    case (?votes) {
                        for (vote in votes.vals()) {
                            voteCount[vote.optionIndex] += 1;
                        };
                    };
                };

                let results = Array.tabulate<(Nat, Nat)>(
                    poll.options.size(),
                    func (i) { (i, voteCount[i]) }
                );
                #ok(results)
            };
        }
    };


    public query func getUserVote(pollId: PollId, userId: UserId): async Result.Result<Vote, Error> {
        let voteKey = makeVoteKey(pollId, userId);
        switch (votes.get(voteKey)) {
            case null { #err(#PollNotFound) };
            case (?vote) { #ok(vote) };
        }
    };

    public query func getActivePolls(): async [Poll] {
        let currentTime = Time.now();
        Array.filter<Poll>(
            Iter.toArray(polls.vals()),
            func (poll) { poll.isActive and currentTime <= poll.endTime }
        )
    };
}


