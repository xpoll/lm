package cn.blmdz.hunt.engine.request;

import java.io.IOException;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Strings;
import com.google.common.base.Supplier;
import com.google.common.base.Throwables;
import com.google.common.net.MediaType;

import cn.blmdz.hunt.common.UserNotLoginException;
import cn.blmdz.hunt.engine.PageRender;
import cn.blmdz.hunt.engine.ThreadVars;
import cn.blmdz.hunt.engine.exception.NotFound404Exception;
import cn.blmdz.hunt.engine.exception.Server500Exception;
import cn.blmdz.hunt.engine.exception.UnAuthorize401Exception;
import cn.blmdz.hunt.engine.service.ConfigService;

@Component
public class ViewRender {
	private static final Logger log = LoggerFactory.getLogger(ViewRender.class);
	@Autowired
	private PageRender pageRender;
	@Autowired
	private ConfigService configService;

	public void view(final String domain, final String path, HttpServletRequest request, HttpServletResponse response,
			final Map<String, Object> context) {
		Supplier<String> getHtml = new Supplier<String>() {
			public String get() {
				return ViewRender.this.pageRender.render(domain, path, context);
			}
		};
		this.render(request, response, getHtml);
	}

	public void render(HttpServletRequest request, HttpServletResponse response, Supplier<String> getHtml) {
		String html = "";

		try {
			html = Strings.nullToEmpty(getHtml.get());
		} catch (UserNotLoginException var10) {
			try {
				response.sendRedirect((String) this.configService.getFrontConfig(ThreadVars.getAppKey())
						.getCurrentHrefs(ThreadVars.getHost()).get("login"));
				return;
			} catch (IOException var9) {
				;
			}
		} catch (Exception var11) {
			Throwables.propagateIfInstanceOf(var11, NotFound404Exception.class);
			Throwables.propagateIfInstanceOf(var11, Server500Exception.class);
			Throwables.propagateIfInstanceOf(var11, UnAuthorize401Exception.class);
			log.error("render failed, cause:{}", Throwables.getStackTraceAsString(Throwables.getRootCause(var11)));
			throw new Server500Exception(var11.getMessage(), var11);
		}

		if (html.startsWith("forward:")) {
			String forwardPath = html.substring("forward:".length());

			try {
				request.getRequestDispatcher(forwardPath).forward(request, response);
			} catch (Exception var7) {
				log.warn("error when forward: {}", html, var7);
			}

		} else {
			response.setContentType(MediaType.HTML_UTF_8.toString());

			try {
				response.getWriter().write(html);
			} catch (IOException var8) {
				;
			}

		}
	}
}
