// Racket bot!

const prefix = "!", trickPrefix = "!!";
const botWriters = [];
const docstring = `
Bot for Racket tricks.

${prefix}help - print this
${prefix}racket - run racket script inline
${prefix}trick or ${trickPrefix} - run a trick
${prefix}add - add a trick
${prefix}edit - edit a trick
${prefix}info - info about a trick
${prefix}remove - remove a trick
${prefix}source - see source for trick
`.trim();

const client = new (require("discord.js").Client)();
const fs = require("fs");

const messageQueue = [];
let rxBuffer = "";

const trickFile = "tricks.json";
let tricks = {}, tricksDirty = false;
try {
	tricks = JSON.parse(fs.readFileSync(trickFile));
} catch (e) {}

const trickContext = (message, name) => tricks[message.guild.id] = (tricks[message.guild.id] || {});

const writeTricks = () => {
	if (tricksDirty) {
		console.log("Writing tricks!");
		fs.writeFileSync(trickFile, JSON.stringify(tricks));
		tricksDirty = false;
	}
};
setInterval(writeTricks, 60 * 1000);

const evaluatorProcess = require("child_process").spawn("racket", ["./evaluator.rkt"], {
	stdio: ["pipe", "pipe", "inherit"]
});

client.on("ready", () => console.log(`Logged in as ${client.user.tag}!`));

const cleanCode = body => {
	if (body.startsWith("```") && body.endsWith("```")) {
		return body.slice(body.indexOf("\n"), -3).trim();
	}
	return body;
}

const enqueue = (message, body, args) => {
	args = args || [];
	const bodyClean = cleanCode(body);
	if (bodyClean) {
		messageQueue.push({message});
		evaluatorProcess.stdin.write(JSON.stringify({args, body: bodyClean, id: message.guild.id}) + "\n");
	}
};

const canWrite = (trick, author) => {
	return author.hasPermission("MANAGE_MESSAGES") || ~botWriters.indexOf(author.id) || trick.author === author.id;
}

const makeTrick = (message, body) => ({author: message.author.id, body, created: message.createdTimestamp});
const writeTrick = (context, name, body) => {
	context[name] = body;
	tricksDirty = true;
};

const functions = {
	help: message => message.channel.send(docstring),
	racket: enqueue,
	add: (message, text) => {
		const [name, body] = shift(text);
		const context = trickContext(message);
		if (context[name]) {
			return message.channel.send(`Trick ${name} already exists, use edit to overwrite!`);
		}
		if (!body) {
			return message.channel.send(`Trick ${name} needs a body!`);
		}
		writeTrick(context, name, makeTrick(message, body));
		return message.channel.send(`Created trick ${name}!`);
	},
	edit: (message, text) => {
		const [name, body] = shift(text);
		const context = trickContext(message), trick = context[name];
		if (!trick) {
			return message.channel.send(`Trick ${name} doesn't exist yet. Use add to make it!`);
		}
		if (!body) {
			return message.channel.send(`Trick ${name} needs a body!`);
		}
		if (!canWrite(trick, message.member)) {
			return message.channel.send(`You are not allowed to edit trick ${name}!`);
		}
		writeTrick(context, name, makeTrick(message, body));
		return message.channel.send(`Updated trick ${name}!`);
	},
	info: (message, text) => {
		const [name, body] = shift(text);
		const trick = trickContext(message)[name];
		if (!trick) {
			return message.channel.send(`Trick ${name} doesn't exist!`);
		}
		const author = client.users.resolve(trick.author);
		return message.channel.send('', {embed: {
			title: `${name}: ${trick.body.length} char${trick.body.length == 1 ? '' : s}`,
			description: `Defined in ${message.guild}`,
			author: {
				name: author && author.tag,
				icon_url: author && author.avatarURL(),
			},
			timestamp: trick.created
		}});
	},
	source: (message, text) => {
		const [name, body] = shift(text);
		const trick = trickContext(message)[name];
		if (!trick) {
			return message.channel.send(`Trick ${name} doesn't exist!`);
		}
		return message.channel.send(trick.face, {code: "lisp"});
	},
	remove: (message, text) => {
		const [name, body] = shift(text);
		const context = trickContext(message), trick = context[name];
		if (!trick) {
			return message.channel.send(`Trick ${name} doesn't exist!`);
		}
		if (!canWrite(trick, message.member)) {
			return message.channel.send(`You are not allowed to delete trick ${name}!`);
		}
		writeTrick(context, name, undefined);
		return message.channel.send(`Removed trick ${name}!`);
	},
	trick: (message, text) => {
		const [name, args] = shift(text);
		const trick = trickContext(message)[name];
		if (!trick) {
			return message.channel.send(`Trick ${name} doesn't exist!`);
		}
		return enqueue(message, trick.body, args && args.split(/\s+/));
	}
};

const shift = text => {
	const pos = text.search(/\s/);
	return ~pos ? [text.substring(0, pos), text.substring(pos).trim()] : [text, ""];
};

const dequeue = line => {
	const channel = messageQueue.shift().message.channel;
	const [opcode, result] = shift(line);
	console.log(result);
	channel.send(JSON.parse(result), {code: "lisp"});
};

client.on("message", msg => {
	if (msg.author.bot || msg.channel.type !== "text") {
		return;
	}
	if (msg.content.startsWith(trickPrefix)) {
		functions.trick(msg, msg.content.substring(trickPrefix.length).trim());
		return;
	}
	if (msg.content.startsWith(prefix)) {
		const [action, body] = shift(msg.content.substring(prefix.length).trim());
		if (functions[action]) functions[action](msg, body);
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


process.on("SIGINT", process.exit);
process.on("exit", writeTricks);
process.on("uncaughtException", writeTricks);

client.login(process.env.BOT_TOKEN.trim());
