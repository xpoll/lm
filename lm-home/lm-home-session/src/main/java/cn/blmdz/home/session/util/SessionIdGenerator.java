package cn.blmdz.home.session.util;

import javax.servlet.http.HttpServletRequest;

public interface SessionIdGenerator {
   String generateId(HttpServletRequest var1);
}
