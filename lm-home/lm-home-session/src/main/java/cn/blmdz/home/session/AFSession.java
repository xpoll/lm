package cn.blmdz.home.session;

import com.google.common.base.Preconditions;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import cn.blmdz.home.session.AFSessionManager;

import java.util.Collections;
import java.util.Enumeration;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionContext;

public class AFSession implements HttpSession {
   private volatile long lastAccessedAt;
   private final AFSessionManager sessionManager;
   private final String prefix;
   private final String id;
   private int maxInactiveInterval;
   private final long createdAt;
   private final HttpServletRequest request;
   private final Map newAttributes = Maps.newHashMapWithExpectedSize(5);
   private final Set<String> deleteAttribute = Sets.newHashSetWithExpectedSize(5);
   private final Map dbSession;
   private volatile boolean invalid;
   private volatile boolean dirty;

   public AFSession(AFSessionManager sessionManager, HttpServletRequest request, String prefix, String id) {
      this.request = request;
      this.sessionManager = sessionManager;
      this.id = id;
      this.createdAt = System.currentTimeMillis();
      this.lastAccessedAt = this.createdAt;
      this.prefix = prefix;
      this.dbSession = this.loadDBSession();
   }

   private Map loadDBSession() {
      return this.sessionManager.findSessionById(this.prefix, this.id);
   }

   public long getCreationTime() {
      return this.createdAt;
   }

   public long getLastAccessedTime() {
      return this.lastAccessedAt;
   }

   public ServletContext getServletContext() {
      return this.request.getServletContext();
   }

   /** @deprecated */
   public HttpSessionContext getSessionContext() {
      return null;
   }

   public Object getAttribute(String name) {
      this.checkValid();
      return this.newAttributes.containsKey(name)?this.newAttributes.get(name):(this.deleteAttribute.contains(name)?null:this.dbSession.get(name));
   }

   /** @deprecated */
   public Object getValue(String name) {
      return this.getAttribute(name);
   }

   public Enumeration getAttributeNames() {
      this.checkValid();
      Set<String> names = Sets.newHashSet(this.dbSession.keySet());
      names.addAll(this.newAttributes.keySet());
      names.removeAll(this.deleteAttribute);
      return Collections.enumeration(names);
   }

   /** @deprecated */
   public String[] getValueNames() {
      this.checkValid();
      Set<String> names = Sets.newHashSet(this.dbSession.keySet());
      names.addAll(this.newAttributes.keySet());
      names.removeAll(this.deleteAttribute);
      return (String[])names.toArray(new String[names.size()]);
   }

   public void setAttribute(String name, Object value) {
      this.checkValid();
      if(value != null) {
         this.newAttributes.put(name, value);
         this.deleteAttribute.remove(name);
      } else {
         this.deleteAttribute.add(name);
         this.newAttributes.remove(name);
      }

      this.dirty = true;
   }

   /** @deprecated */
   public void putValue(String name, Object value) {
      this.setAttribute(name, value);
   }

   public void removeAttribute(String name) {
      this.checkValid();
      this.deleteAttribute.add(name);
      this.newAttributes.remove(name);
      this.dirty = true;
   }

   /** @deprecated */
   public void removeValue(String name) {
      this.removeAttribute(name);
      this.dirty = true;
   }

   public void invalidate() {
      this.invalid = true;
      this.dirty = true;
      this.sessionManager.deletePhysically(this.prefix, this.getId());
   }

   public boolean isNew() {
      return true;
   }

   protected void checkValid() throws IllegalStateException {
      Preconditions.checkState(!this.invalid);
   }

   public Map snapshot() {
      Map<String, Object> snap = Maps.newHashMap();
      snap.putAll(this.dbSession);
      snap.putAll(this.newAttributes);

      for(String name : this.deleteAttribute) {
         snap.remove(name);
      }

      return snap;
   }

   public boolean isValid() {
      return !this.invalid;
   }

   public String getPrefix() {
      return this.prefix;
   }

   public String getId() {
      return this.id;
   }

   public int getMaxInactiveInterval() {
      return this.maxInactiveInterval;
   }

   public void setMaxInactiveInterval(int maxInactiveInterval) {
      this.maxInactiveInterval = maxInactiveInterval;
   }

   public boolean isDirty() {
      return this.dirty;
   }
}
