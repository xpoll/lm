package cn.blmdz.hunt.engine.request;

import java.io.IOException;
import java.util.Map;

import javax.annotation.PostConstruct;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.net.MediaType;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.util.Joiners;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.hunt.engine.ActionEngine;
import cn.blmdz.hunt.engine.ActionInvoker;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.hunt.engine.config.model.Mapping;
import cn.blmdz.hunt.engine.mapping.Invoker;
import cn.blmdz.hunt.engine.security.CSRFHelper;
import cn.blmdz.hunt.engine.utils.CookieBuilder;
import cn.blmdz.hunt.engine.utils.Domains;
import cn.blmdz.hunt.engine.utils.LoginInfo;
import cn.blmdz.hunt.protocol.Action;
import cn.blmdz.hunt.protocol.action.LoginAction;
import cn.blmdz.hunt.protocol.action.LogoutAction;

@Component
public class MappingHandler {
	@Autowired
	protected Invoker invoker;
	@Autowired
	protected ActionEngine actionEngine;
	@Autowired
	private CSRFHelper csrfHelper;

	@PostConstruct
	public void init() {
		this.actionEngine.registerOnce(LoginAction.class, new ActionInvoker<LoginAction>() {
			public boolean invoke(LoginAction loginAction, HttpServletRequest request, HttpServletResponse response) {
				if (loginAction.getUserId() == null) {
					return false;
				} else {
					LoginInfo loginInfo = new LoginInfo(loginAction.getUserId(), request.getRemoteAddr());
					String domain = ThreadVars.getDomain().equals("localhost") ? null
							: "." + Domains.removeSubDomain(ThreadVars.getDomain());
					Cookie cookie = CookieBuilder.from("pms_id", loginInfo.toCookieKey(), domain).httpOnly()
							.maxAge(loginAction.getMaxAge().intValue()).build();
					response.addCookie(cookie);
					return false;
				}
			}
		});
		this.actionEngine.registerOnce(LogoutAction.class, new ActionInvoker<LogoutAction>() {
			public boolean invoke(LogoutAction action, HttpServletRequest request, HttpServletResponse response) {
				String domain = ThreadVars.getDomain().equals("localhost") ? null
						: "." + Domains.removeSubDomain(ThreadVars.getDomain());
				Cookie cookie = CookieBuilder.from("pms_id", (String) null, domain).httpOnly().maxAge(0).build();
				response.addCookie(cookie);
				return false;
			}
		});
	}

	public boolean handle(String path, HttpServletRequest request, HttpServletResponse response,
			Map<String, String> context) {
		return this.handle(ThreadVars.getAppKey(), path, request, response, context);
	}

	protected boolean handle(String app, String path, HttpServletRequest request, HttpServletResponse response,
			Map<String, String> context) {
		String method = request.getMethod().toUpperCase();
		Mapping mapping = this.invoker.mappingMatch(app, Joiners.COLON.join(method, path, new Object[0]));
		if (mapping == null) {
			return false;
		} else if (mapping.isCsrfCheck() && !this.csrfHelper.check(request)) {
			throw new JsonResponseException(403, "Duplicate request");
		} else {
			response.setContentType(MediaType.JSON_UTF_8.toString());
			Object result = this.invoker.mappingInvoke(mapping, path, context);
			if (result instanceof Action && this.actionEngine.handler((Action) result, request, response)) {
				return true;
			} else {
				Object realResult = result instanceof Action ? ((Action) result).getData() : result;

				try {
					response.getWriter().write(JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(realResult));
				} catch (IOException var11) {
					;
				}

				return true;
			}
		}
	}
}
