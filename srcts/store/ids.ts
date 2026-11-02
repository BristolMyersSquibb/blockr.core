// Ids, which the client alone mints, in the adjective and animal style of R's
// rand_names(), for the objects and boards it creates and for each object of a
// board that R sends without one, so that ids come from one place.

// An id is any string that is neither empty nor made of digits alone, since a
// JS object puts keys that look like integers first, whatever the order they
// were inserted in.
export function isId(id: unknown): boolean {
  return typeof id === "string" && id !== "" && !/^\d+$/.test(id);
}

// A new id, such as "curious_otter", that `taken` does not hold.
export function mintId(taken: (id: string) => boolean): string {
  for (let i = 0; i < 100; i++) {
    const id = `${pick(adjectives)}_${pick(animals)}`;
    if (!taken(id)) return id;
  }

  const id = `${pick(adjectives)}_${pick(animals)}`;
  let n = 2;
  while (taken(`${id}_${String(n)}`)) n++;
  return `${id}_${String(n)}`;
}

// A name made from an id, "Curious otter" from "curious_otter", as R's
// id_to_sentence_case() makes one.
export function sentenceCase(id: string): string {
  const words = id
    .replace(/([A-Z])/g, " $1")
    .replace(/[_\-.]/g, " ")
    .trim()
    .split(" ");
  const [first = "", ...rest] = words;
  return [
    first.charAt(0).toUpperCase() + first.slice(1).toLowerCase(),
    ...rest.map((word) => word.toLowerCase()),
  ].join(" ");
}

function pick(words: readonly string[]): string {
  return words[Math.floor(Math.random() * words.length)] ?? "";
}

// Words of at most eight letters, as rand_names() draws them.

const adjectives = [
  "able", "active", "agile", "alert", "amber", "ample", "apt", "ardent",
  "astral", "autumn", "avid", "azure", "balmy", "bold", "brave", "breezy",
  "bright", "brisk", "bronze", "bubbly", "busy", "calm", "candid", "careful",
  "cheery", "civil", "clear", "clever", "cloudy", "coastal", "cobalt",
  "cosmic", "cozy", "crimson", "crisp", "curious", "dapper", "daring",
  "dashing", "deft", "dewy", "direct", "distant", "dreamy", "dusky", "eager",
  "early", "earnest", "easy", "elated", "elegant", "eminent", "epic", "even",
  "exact", "fabled", "fair", "famous", "fancy", "fast", "festive", "fiery",
  "fine", "firm", "fleet", "fluent", "fond", "frank", "free", "fresh",
  "frosty", "gentle", "genuine", "giant", "gifted", "glad", "gleeful",
  "glossy", "golden", "good", "grand", "great", "green", "happy", "hardy",
  "hazel", "hearty", "honest", "hopeful", "humble", "icy", "ideal", "indigo",
  "jade", "jolly", "jovial", "joyful", "keen", "kind", "lavish", "leafy",
  "light", "lilac", "lively", "lofty", "loyal", "lucid", "lucky", "lunar",
  "lush", "magic", "major", "mellow", "merry", "mighty", "mild", "misty",
  "modern", "modest", "mossy", "nimble", "noble", "noted", "novel", "oaken",
  "olive", "open", "orange", "patient", "peaceful", "perky", "placid",
  "plain", "plucky", "polar", "polite", "prime", "proper", "proud", "quick",
  "quiet", "radiant", "rapid", "rare", "ready", "regal", "rich", "robust",
  "rosy", "royal", "ruby", "rustic", "sage", "sandy", "savvy", "scarlet",
  "serene", "sharp", "shiny", "silent", "silky", "silver", "simple",
  "sincere", "sleek", "smart", "smooth", "snowy", "social", "solar", "solid",
  "sonic", "spry", "stable", "stately", "steady", "stellar", "still",
  "stoic", "sturdy", "sunny", "superb", "sure", "swift", "tidy", "tranquil",
  "true", "trusty", "upbeat", "urban", "valiant", "vast", "velvet",
  "vibrant", "violet", "vital", "vivid", "warm", "wary", "wise", "witty",
  "woven", "young", "zany", "zealous", "zesty",
];

const animals = [
  "aardvark", "alpaca", "anchovy", "ant", "antelope", "badger", "barnacle",
  "bat", "beagle", "bear", "beaver", "bee", "beetle", "bison", "bobcat",
  "buffalo", "bullfrog", "bunny", "camel", "canary", "caribou", "carp",
  "cat", "catfish", "chamois", "cheetah", "chicken", "chipmunk", "cicada",
  "clam", "cobra", "cod", "condor", "cougar", "cow", "coyote", "crab",
  "crane", "cricket", "crow", "cuckoo", "curlew", "deer", "dingo", "dolphin",
  "donkey", "dove", "duck", "eagle", "eel", "egret", "eland", "elk", "emu",
  "falcon", "ferret", "finch", "firefly", "flamingo", "fox", "frog",
  "gazelle", "gecko", "gerbil", "gibbon", "giraffe", "gnu", "goat", "goose",
  "gopher", "gorilla", "grouse", "gull", "hamster", "hare", "hawk",
  "hedgehog", "heron", "herring", "hippo", "hornet", "horse", "husky",
  "ibex", "ibis", "iguana", "impala", "jackal", "jaguar", "jay", "kangaroo",
  "kestrel", "kiwi", "koala", "krill", "ladybug", "lamb", "lemming", "lemur",
  "leopard", "lion", "lizard", "llama", "lobster", "loon", "lynx", "macaw",
  "magpie", "mallard", "manatee", "mantis", "marmot", "marten", "meerkat",
  "mink", "mole", "mongoose", "monkey", "moose", "moth", "mouse", "mule",
  "narwhal", "newt", "ocelot", "octopus", "okapi", "opossum", "orca",
  "oriole", "osprey", "ostrich", "otter", "owl", "ox", "oyster", "panda",
  "panther", "parrot", "peacock", "pelican", "penguin", "pheasant",
  "pigeon", "pika", "pike", "plover", "pony", "porpoise", "possum",
  "puffin", "puma", "python", "quail", "rabbit", "raccoon", "ram", "raven",
  "reindeer", "rhino", "robin", "rooster", "salmon", "sardine", "seahorse",
  "seal", "shark", "sheep", "shrew", "shrimp", "skunk", "sloth", "snail",
  "sparrow", "squid", "starling", "stingray", "stork", "swallow", "swan",
  "tapir", "termite", "tern", "tiger", "toad", "toucan", "trout", "tuna",
  "turkey", "turtle", "vole", "wallaby", "walrus", "warthog", "wasp",
  "weasel", "whale", "wolf", "wombat", "woodcock", "wren", "yak", "zebra",
];
