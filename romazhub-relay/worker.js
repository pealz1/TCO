// RomazHub Relay Worker — Cloudflare Workers
// Stores active hub users per jobId with auto-expiry.
// No KV needed — uses in-memory Map (resets on cold start, which is fine).

const EXPIRY_MS = 90_000; // 90 seconds without heartbeat = dead
const sessions = new Map(); // jobId -> Map<userId, {timestamp, placeId}>

function cleanup() {
  const now = Date.now();
  for (const [jobId, users] of sessions) {
    for (const [userId, entry] of users) {
      if (now - entry.timestamp > EXPIRY_MS) {
        users.delete(userId);
      }
    }
    if (users.size === 0) {
      sessions.delete(jobId);
    }
  }
}

export default {
  async fetch(request) {
    cleanup();

    const url = new URL(request.url);
    const path = url.pathname;

    // CORS headers for Roblox HTTP requests
    const headers = {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, DELETE, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type",
    };

    if (request.method === "OPTIONS") {
      return new Response(null, { status: 204, headers });
    }

    // POST /heartbeat — client checks in
    // Body: { jobId, userId, placeId }
    if (request.method === "POST" && path === "/heartbeat") {
      try {
        const body = await request.json();
        const { jobId, userId, placeId } = body;

        if (!jobId || !userId) {
          return new Response(
            JSON.stringify({ error: "missing jobId or userId" }),
            { status: 400, headers }
          );
        }

        if (!sessions.has(jobId)) {
          sessions.set(jobId, new Map());
        }

        sessions.get(jobId).set(String(userId), {
          timestamp: Date.now(),
          placeId: placeId || 0,
        });

        // Return the current user list for this jobId immediately
        const users = [];
        for (const [uid, entry] of sessions.get(jobId)) {
          users.push(Number(uid));
        }

        return new Response(
          JSON.stringify({ ok: true, users }),
          { status: 200, headers }
        );
      } catch (e) {
        return new Response(
          JSON.stringify({ error: "bad request" }),
          { status: 400, headers }
        );
      }
    }

    // GET /check?jobId=xxx — poll who's in this server
    if (request.method === "GET" && path === "/check") {
      const jobId = url.searchParams.get("jobId");
      if (!jobId) {
        return new Response(
          JSON.stringify({ error: "missing jobId" }),
          { status: 400, headers }
        );
      }

      const users = [];
      if (sessions.has(jobId)) {
        for (const [uid, entry] of sessions.get(jobId)) {
          users.push(Number(uid));
        }
      }

      return new Response(
        JSON.stringify({ ok: true, users }),
        { status: 200, headers }
      );
    }

    // DELETE /leave — client disconnects cleanly
    // Body: { jobId, userId }
    if (request.method === "DELETE" && path === "/leave") {
      try {
        const body = await request.json();
        const { jobId, userId } = body;

        if (jobId && userId && sessions.has(jobId)) {
          sessions.get(jobId).delete(String(userId));
          if (sessions.get(jobId).size === 0) {
            sessions.delete(jobId);
          }
        }

        return new Response(
          JSON.stringify({ ok: true }),
          { status: 200, headers }
        );
      } catch (e) {
        return new Response(
          JSON.stringify({ error: "bad request" }),
          { status: 400, headers }
        );
      }
    }

    // Health check
    if (path === "/" || path === "/health") {
      return new Response(
        JSON.stringify({ status: "ok", sessions: sessions.size }),
        { status: 200, headers }
      );
    }

    return new Response(
      JSON.stringify({ error: "not found" }),
      { status: 404, headers }
    );
  },
};
