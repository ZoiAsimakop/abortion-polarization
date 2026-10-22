<!-- Guide to the Shiny app by Soma Breitner. Included at the end of main.qmd. -->

This is a community where we love to discuss abortion and polarize about it. Since we are a very proud bunch of Haters, we made a simulation to show you how amazing we are when it comes to debating social issues. We are not so excited about the issue of abortion as much as about the sheer thrill of hating on each other and telling our neighbours that they are surely wrong about everything they believe in!

Let's see how we behave, using the Shiny app that we created. We like to refer to ourselves in fancy terms, so from now on, we will call ourselves "agents". So run the app, buckle your seatbelts and let us walk you through this town of hate.

You opened the Shiny app? Great! Isn't it beautiful?

### The rules of our town (and the Shiny app)

#### Who we are, how many of us, and when?

- In the top-left corner, you can choose the number of agents and time steps. You can also set different seeds to see our fellow neighbouring Hater towns.
- After you've decided on these, scroll down and push **Run simulation** to see our beautiful town before your very eyes.
- To see how polarized we get about abortion as time progresses and we discuss the issue more and more, use the **Displayed time point** slider. Not very impressive, is it? We seem to move towards neutral opinions. That can't be right. How can we change this?

#### Use the sliders to make us more hateful

- **Typical conviction:** You can decide how convinced we should be about our views. Generally, we are more interested in telling each other off than in the subject matter itself, so we set it to a default value of 0.5. If you increase it, we will change our opinions less.
- **Open-minded agent share:** Similar to conviction, but a little different. Open-minded agents accept messages from a wider range of opinions, so they are more sensitive to campaigns by other agents in the community. If you increase the open-minded agent share, campaigns (introduced later) will affect more agents. Are these people actually interested in the debate rather than power? Such fools!
- **Zealot share:** Now we get to the nice, decent people: the zealots. These are the most awful, self-loaded and, most importantly, stubborn people I know. They have very radical opinions and would not change for a billion euros. These are the little squares you see on the left and right sides of the opinion motion plot. Whatever settings you change, these agents will just stay where they are. If you like polarization as much as we do, you can increase their share in our beautiful community.
- **Campaign position:** For some obscure reason, some agents in our community care about the topic of abortion more than about being right. Unfortunately, they tend to create campaigns. These are sometimes pro-choice and at other times pro-life. You can choose which campaign you'd like to see with the click of a button. This affects the direction in which agents are likely to polarize.
- **Activist share:** Here come the weirdos: activists. You can increase their number in our community. If you do so, the campaign becomes more visible and polarization will increase.
- **Campaign intensity:** Campaigns are not always intense. It depends on when you visit our beautiful little town. A more intense campaign will lead to more polarization.
- **Enable backlash to reject campaign messages:** You may have noticed that even though you adjust the previous sliders, a lot of agents still remain neutral. That's no way to live, so our amazing, petty little hateful town invented a beautiful concept: backlash. This means that if activists campaign, we start moving to the opposite opinion of what they are trying to propagate. Yes, this is out of pure spite, and we love it. We love it so much that if you turn this option on, more of us will adopt the opposite opinion to the campaign's position. Please turn it on, because this is what we live for! You will notice that with backlash on, we polarize much more.
- **Backlash strength:** Okay, we don't always backlash, so we added a slider so you can adjust how much we do it. The bottom part of the scale is very sensitive, so we recommend starting with a value of 0.05 and increasing it gradually if you are as much of a Hater as we are. At higher values, we just polarize quickly. That's just in our nature.
- **External information exposure rate:** Recently, we failed to stop a new invention from coming into our modest town: television. Wouldn't you agree that it's just an awful piece of machinery? Oh, you do? Then I actually think it's the greatest invention of all time! See? You've just been backlashed. Our agents watch this TV thing, and this damn machine for some reason tells them what to think. With the information exposure rate, you can decide how many families in our cool little town are exposed to messages about abortion on television. The more you increase this slider, the more effect the external message position has.
- **External message position:** This decides what kind of message people hear. The message can be pro-life, pro-choice or even neutral. Make sure to check out the neutral setting with a high external information exposure rate and a high external information uptake. This way, you can make our agents consider arguments from both sides and adopt radically neutral opinions. For this, it might be a good idea to turn off backlash or turn down campaign intensity or activist share. If you leave these high, they tend to cancel out the effect of external messages. I guess interactions with the people around you influence your opinions more than TV. Who would have thought? External information generally has a weaker effect in the model.
- **External information uptake:** This decides how much the agents watching that absurd TV of theirs believe what's said there. The higher the external information uptake, the more effect the external message position has on agents' opinions.

#### Some notes about the sliders

- You might sometimes get surprising effects. In these cases, it is good to tinker with several sliders and see if you can get the effect you are looking for. Since our town's research question was about backlash, it often helps to turn on backlash when you want to see a polarization effect.
- Most of the time without backlash, a lot of agents won't polarize. This is realistic: even though we are the Hater town, most people in society are probably content being part of the silent neutral majority, but our best Haters will make sure their voices are heard and make it seem like the world is extremely polarized.
- To inspire our hate, we studied the book [*Complex Systems Research in Psychology*](https://santafeinstitute.github.io/ComplexPsych/) by Han van der Maas (2024). Even Haters need a good textbook.

#### Visual beauty and tabs

- To make the opinion motion more appealing to you, you can adjust the trail length and agent sizes.
- Below the opinion motion, you see an explanation of agent shapes and colours.
- Use the **Opinions** and **Agents** tabs to see some data about our agents. You can also hover over specific agents in the opinion motion to see their data. For any uncertainties about the data, see the R code.

Thank you for visiting our one-of-a-kind town! We hope you enjoyed it, and until next time.

Best,

The Haters
