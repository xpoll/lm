package cn.blmdz.wolf.auth.web.helper;

import java.io.IOException;
import java.util.regex.Pattern;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.stereotype.Component;

import com.github.jknack.handlebars.Helper;
import com.github.jknack.handlebars.Options;
import com.github.jknack.handlebars.TagType;

import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.engine.handlebars.HandlebarsEngine;
import cn.blmdz.wolf.auth.core.Authenticator;
import cn.blmdz.wolf.common.model.ParanaUser;

@Component
@ConditionalOnClass({ HandlebarsEngine.class })
@ConditionalOnBean({ HandlebarsEngine.class })
public class ExtraHbsHelpers {
	@Autowired
	public ExtraHbsHelpers(HandlebarsEngine handlebarsEngine, final Authenticator authenticator) {
		handlebarsEngine.registerHelper("withPerm", new Helper<String>() {
			public CharSequence apply(String context, Options options) throws IOException {
				boolean success = authenticator.ask((ParanaUser) UserUtil.getCurrentUser(), context);
				if (options.tagType == TagType.SECTION) {
					return success ? options.fn() : options.inverse();
				} else {
					String successPhrase = (String) options.hash("success", "");
					String failPhrase = (String) options.hash("fail", "hidden");
					return success ? successPhrase : failPhrase;
				}
			}
		});
		handlebarsEngine.registerHelper("hasRole", new Helper<String>() {
			public CharSequence apply(String key, Options options) throws IOException {
				BaseUser user = UserUtil.getCurrentUser();
				if (user != null && user.getRoles() != null && !user.getRoles().isEmpty()) {
					Pattern pat = Pattern.compile(key);

					for (String role : user.getRoles()) {
						if (pat.matcher(role).matches()) {
							return options.fn();
						}
					}

					return options.inverse();
				} else {
					return options.inverse();
				}
			}
		});
	}
}
