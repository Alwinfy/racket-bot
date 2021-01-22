// Racket bot!

const prefix = "racket!";

const client = new (require("discord.js").Client)();

const messageQueue = [];
let rxBuffer = "";

const evaluatorProcess = require("child_process").spawn("racket", ["./evaluator.rkt"], {
	stdio: ["pipe", "pipe", "inherit"]
});

client.on("ready", () => console.log(`Logged in as ${client.user.tag}!`));

const enqueue = (message, body) => {
	messageQueue.push({message, body});
	evaluatorProcess.stdin.write(JSON.stringify(body) + "\n");
};

const dequeue = line => {
	const channel = messageQueue.shift().message.channel;
	const spacePos = line.indexOf(" ");
	const opcode = line.substring(0, spacePos), result = line.substring(spacePos + 1);
	console.log(result);
	channel.sendCode("racket", JSON.parse(result));
};

client.on("message", msg => {
	if (msg.author.bot || !msg.content.startsWith(prefix)) {
		return;
	}
	let rawCode = msg.content.substring(prefix.length).trim();
	if (rawCode.startsWith("```") && rawCode.endsWith("```")) {
		rawCode = rawCode.slice(rawCode.indexOf("\n"), -3).trim();
	}
	if (rawCode) {
		enqueue(msg, rawCode);
	}
});

evaluatorProcess.on("error", console.error);

evaluatorProcess.stdout.on("data", data => {
	const lines = (rxBuffer + data.toString()).split("\n");
	for (let i = 0; i < lines.length - 1; i++) {
		dequeue(lines[i]);
	}
	rxBuffer = lines[lines.length - 1];
});

if (!process.env.BOT_TOKEN) {
	throw new Error("Missing environment variable BOT_TOKEN!");
}

client.login(process.env.BOT_TOKEN.trim());
